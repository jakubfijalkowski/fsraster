module FsRaster.D3.LowLevelRendering

open System.Diagnostics.CodeAnalysis

open FsRaster
open FsRaster.Utils
open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Model
open FsRaster.D3.Renderer


// Unfortunately, having global (ie. bound to renderer) z-buffer makes it usable.
// Allocating it for every frame is extremely inefficient.

#nowarn "9"

let inline private clampScreen m v = min (max 0 v) (m - 1)

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private renderLine ctx (v1 : Vector4) (v2 : Vector4) c =
    let x1' = int v1.X
    let x2' = int v2.X
    let y1' = int v1.Y
    let y2' = int v2.Y
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    if x1' = x2' && y1' = y2' then
        if x1' >= 0 && x1' < w && y1' >= 0 && y1' < h then
            NativeInterop.NativePtr.set pixels (x1' + y1' * w) c
    else
        let (x1, y1, x2, y2), transform =
            match (x2' - x1', y2' - y1') with
            | dx, dy when            dy >= 0 &&  dx >=  dy -> ( x1',  y1',  x2',  y2'), (fun x y -> ( x,  y))
            | dx, dy when dx >= 0 &&             dy >   dx -> ( y1',  x1',  y2',  x2'), (fun x y -> ( y,  x))
            | dx, dy when dx <  0 && dy >= 0 && -dx >=  dy -> (-x1',  y1', -x2',  y2'), (fun x y -> (-x,  y))
            | dx, dy when dx <  0 &&             dy >  -dx -> ( y1', -x1',  y2', -x2'), (fun x y -> (-y,  x))
            | dx, dy when dx <  0 && dy <  0 && -dx >= -dy -> (-x1', -y1', -x2', -y2'), (fun x y -> (-x, -y))
            | dx, dy when dx <  0 && dy <  0 && -dy >  -dx -> (-y1', -x1', -y2', -x2'), (fun x y -> (-y, -x))
            | dx, dy when dx >= 0 && dy <  0 && -dy >=  dx -> (-y1',  x1', -y2',  x2'), (fun x y -> ( y, -x))
            | dx, dy when dx >  0 && dy <  0 &&  dx >  -dy -> ( x1', -y1',  x2', -y2'), (fun x y -> ( x, -y))
            | _ -> (0, 0, 0, 0), (fun x y -> (x, y))
        let dx = x2 - x1
        let dy = y2 - y1
        let d' = 2 * dy - dx
        let incE = 2 * dy
        let incNE = 2 * (dy - dx)
        let rec build d x y =
            if x <= x2 then 
                let x', y' = transform x y 
                if x' >= 0 && x' < w && y' >= 0 && y' < h then
                    NativeInterop.NativePtr.set pixels (x' + y' * w) c
                if d < 0 then build (d + incE) (x + 1) y
                else build (d + incNE) (x + 1) (y + 1)
        build d' x1 y1
        ()

// Scanline algorithm, but adjusted to triangles in 3D - should be faster than scanline from FsRaster.FigureRendering
let inline private sortByY (t : RenderTriangle) =
    let arr = [| (t.V1, t.N1); (t.V2, t.N2); (t.V3, t.N3) |]
    Array.sortInPlaceBy (fun (v, _) -> v.Y) arr
    { t with
          V1 = fst arr.[0]; N1 = snd arr.[0];
          V2 = fst arr.[1]; N2 = snd arr.[1];
          V3 = fst arr.[2]; N3 = snd arr.[2] }

type ActiveEdge =
    {
        YMin : int;
        YMax : int;

        mutable X : double;
        mutable Z : double;
        mutable Normal : Vector3;

        CoeffX : double;
        CoeffZ : double;
        CoeffNormal : Vector3
    }

let inline private buildTopTriangle t =
    let dy = (t.V1.Y - t.V3.Y)
    let mx1 = (t.V1.X - t.V3.X) / dy
    let mz1 = (t.V1.Z - t.V3.Z) / dy
    let mx2 = (t.V2.X - t.V3.X) / dy
    let mz2 = (t.V2.Z - t.V3.Z) / dy

    let mn1 = (t.N1 - t.N3) / dy
    let mn2 = (t.N2 - t.N3) / dy

    let ae1 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V3.Y;

            X = t.V1.X;
            Z = t.V1.Z;
            Normal = t.N1;

            CoeffX = mx1;
            CoeffZ = mz1;
            CoeffNormal = mn1
        }
    let ae2 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V3.Y;

            X = t.V2.X;
            Z = t.V2.Z;
            Normal = t.N2;

            CoeffX = mx2;
            CoeffZ = mz2;
            CoeffNormal = mn2
        }
    (ae1, ae2)

let inline private buildBottomTriangle t =
    let dy = (t.V1.Y - t.V3.Y)
    let mx1 = (t.V1.X - t.V3.X) / dy
    let mz1 = (t.V1.Z - t.V3.Z) / dy
    let mx2 = (t.V1.X - t.V2.X) / dy
    let mz2 = (t.V1.Z - t.V2.Z) / dy

    let mn1 = (t.N1 - t.N3) / dy
    let mn2 = (t.N1 - t.N2) / dy

    let ae1 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V3.Y;

            X = t.V1.X;
            Z = t.V1.Z;
            Normal = t.N1;

            CoeffX = mx1;
            CoeffZ = mz1
            CoeffNormal = mn1
        }
    let ae2 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V3.Y;

            X = t.V1.X;
            Z = t.V1.Z;
            Normal = t.N1;

            CoeffX = mx2;
            CoeffZ = mz2;
            CoeffNormal = mn2
        }
    (ae1, ae2)

let inline private buildProperTriangle t =
    let dy13 = (t.V1.Y - t.V3.Y)
    let dy12 = (t.V1.Y - t.V2.Y)
    let dy23 = (t.V2.Y - t.V3.Y)
    let mx1 = (t.V1.X - t.V3.X) / dy13
    let mz1 = (t.V1.Z - t.V3.Z) / dy13
    let mx2 = (t.V1.X - t.V2.X) / dy12
    let mz2 = (t.V1.Z - t.V2.Z) / dy12
    let mx3 = (t.V2.X - t.V3.X) / dy23
    let mz3 = (t.V2.Z - t.V3.Z) / dy23

    let mn1 = (t.N1 - t.N3) / dy13
    let mn2 = (t.N1 - t.N2) / dy12
    let mn3 = (t.N2 - t.N3) / dy23

    let midX = t.V1.X - mx1 * dy12
    let midN = t.N1 - mn1 * dy12

    let ae1 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V2.Y;

            X = t.V1.X;
            Z = t.V1.Z;
            Normal = t.N1;

            CoeffX = mx1;
            CoeffZ = mz1;
            CoeffNormal = mn1
        }
    let ae2 =
        {
            YMin = int t.V1.Y;
            YMax = int t.V2.Y;

            X = t.V1.X;
            Z = t.V1.Z;
            Normal = t.N1;

            CoeffX = mx2;
            CoeffZ = mz2
            CoeffNormal = mn1
        }

    let ae3 =
        {
            YMin = int t.V2.Y;
            YMax = int t.V3.Y;

            X = midX;
            Z = t.V1.Z;
            Normal = midN;

            CoeffX = mx1;
            CoeffZ = mz1
            CoeffNormal = mn1
        }
    let ae4 =
        {
            YMin = int t.V2.Y;
            YMax = int t.V3.Y;

            X = t.V2.X;
            Z = t.V2.Z;
            Normal = t.N2;

            CoeffX = mx3;
            CoeffZ = mz3
            CoeffNormal = mn3
        }
    [ ae1, ae2; ae3, ae4 ]

let private getAEs t' =
    let t = sortByY t'
    if t.V1.Y = t.V2.Y then [ buildTopTriangle t ]
    else if t.V2.Y = t.V3.Y then [ buildBottomTriangle t ]
    else buildProperTriangle t

let private adjustXPositions orgY newY ae1 ae2 =
    if orgY <> newY then
        let diff = double (newY - orgY)
        ae1.X <- ae1.X + diff * ae1.CoeffX
        ae2.X <- ae2.X + diff * ae2.CoeffX

[<SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let private phongShading renderer (t : RenderTriangle) w h x y z (n : Vector3) c =
    let pos = vec3 (double x / w) (double y / h) z
    let l = (pos - renderer.Light.Position).Normalized
    let dt = dot3 l n.Normalized
    let dCoeff = max 0.0 dt
    let aR = int (t.Material.AmbientCoeff * renderer.Light.AmbientR)
    let aG = int (t.Material.AmbientCoeff * renderer.Light.AmbientG)
    let aB = int (t.Material.AmbientCoeff * renderer.Light.AmbientB)
    let dR = int (t.Material.DiffuseCoeff * renderer.Light.DiffuseR * dCoeff)
    let dG = int (t.Material.DiffuseCoeff * renderer.Light.DiffuseG * dCoeff)
    let dB = int (t.Material.DiffuseCoeff * renderer.Light.DiffuseB * dCoeff)
    let r = Colors.clamp (aR + dR + Colors.getR c)
    let g = Colors.clamp (aG + dG + Colors.getG c)
    let b = Colors.clamp (aB + dB + Colors.getB c)
    Colors.fromRGB r g b
    
// This is too much of copy-paste, but extracting small, inlineable methods would not help
// readability and redesigning this would make it less performant

let private renderTriangleAlways renderer ctx t =
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let c = t.Color

    let aes = getAEs t

    for ae1, ae2 in aes do
        let ymin = clampScreen h ae1.YMin
        let ymax = clampScreen h ae1.YMax
        adjustXPositions ae1.YMin ymin ae1 ae2
        for y = ymin to ymax - 1 do
            let minX = clampScreen w (int (min ae1.X ae2.X))
            let maxX = clampScreen w (int (max ae1.X ae2.X))
            for x = minX to maxX do
                NativeInterop.NativePtr.set pixels (x + y * w) c
            ae1.X <- ae1.X + ae1.CoeffX
            ae2.X <- ae2.X + ae2.CoeffX

let private renderTriangleAlwaysPhong renderer ctx t =
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let w' = double w
    let h = ctx.Height
    let h' = double h
    let c = t.Color

    let aes = getAEs t

    for ae1, ae2 in aes do
        let ymin = clampScreen h ae1.YMin
        let ymax = clampScreen h ae1.YMax
        adjustXPositions ae1.YMin ymin ae1 ae2
        for y = ymin to ymax - 1 do
            let minX = clampScreen w (int (min ae1.X ae2.X))
            let maxX = clampScreen w (int (max ae1.X ae2.X))

            let mz = if minX <> maxX then (ae2.Z - ae1.Z) / (ae2.X - ae1.X) else 0.0
            let mutable z = if ae1.X <= ae2.X then ae1.Z else ae2.Z

            let mn = if minX <> maxX then (ae2.Normal - ae1.Normal) / (ae2.X - ae1.X) else vec3Zero
            let mutable normal = if ae1.X <= ae2.X then ae1.Normal else ae2.Normal

            for x = minX to maxX do
                let c' = phongShading renderer t w' h' x y z normal c
                NativeInterop.NativePtr.set pixels (x + y * w) c'

                normal <- vec3Add normal mn
                z <- z + mz

            ae1.X <- ae1.X + ae1.CoeffX
            ae2.X <- ae2.X + ae2.CoeffX

            ae1.Z <- ae1.Z + ae1.CoeffZ
            ae2.Z <- ae2.Z + ae2.CoeffZ

            ae1.Normal <- ae1.Normal + ae1.CoeffNormal
            ae2.Normal <- ae2.Normal + ae2.CoeffNormal

let private renderTriangleZBuffer renderer ctx t = 
    let zBuffer = renderer.ZBuffer
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let c = t.Color

    let aes = getAEs t

    for ae1, ae2 in aes do
        let ymin = clampScreen h ae1.YMin
        let ymax = clampScreen h ae1.YMax
        adjustXPositions ae1.YMin ymin ae1 ae2

        for y = ymin to ymax - 1 do
            let minX = clampScreen w (int (min ae1.X ae2.X))
            let maxX = clampScreen w (int (max ae1.X ae2.X))

            let mz = if minX <> maxX then (ae2.Z - ae1.Z) / (ae2.X - ae1.X) else 0.0
            let mutable z = if ae1.X <= ae2.X then ae1.Z else ae2.Z
            for x = minX to maxX do
                let idx = y * w + x
                if zBuffer.[idx] > z then
                    NativeInterop.NativePtr.set pixels idx c
                    zBuffer.[idx] <- z
                z <- z + mz
            ae1.X <- ae1.X + ae1.CoeffX
            ae1.Z <- ae1.Z + ae1.CoeffZ
            ae2.X <- ae2.X + ae2.CoeffX
            ae2.Z <- ae2.Z + ae2.CoeffZ

let private renderTriangleZBufferPhong renderer ctx t = 
    let zBuffer = renderer.ZBuffer
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let c = t.Color

    let aes = getAEs t

    for ae1, ae2 in aes do
        let ymin = clampScreen h ae1.YMin
        let ymax = clampScreen h ae1.YMax
        adjustXPositions ae1.YMin ymin ae1 ae2

        for y = ymin to ymax - 1 do
            let minX = clampScreen w (int (min ae1.X ae2.X))
            let maxX = clampScreen w (int (max ae1.X ae2.X))

            let mz = if minX <> maxX then (ae2.Z - ae1.Z) / (ae2.X - ae1.X) else 0.0
            let mutable z = if ae1.X <= ae2.X then ae1.Z else ae2.Z
            for x = minX to maxX do
                let idx = y * w + x
                if zBuffer.[idx] > z then
                    NativeInterop.NativePtr.set pixels idx c
                    zBuffer.[idx] <- z
                z <- z + mz
            ae1.X <- ae1.X + ae1.CoeffX
            ae1.Z <- ae1.Z + ae1.CoeffZ
            ae2.X <- ae2.X + ae2.CoeffX
            ae2.Z <- ae2.Z + ae2.CoeffZ

let drawModel renderer' (context : CachedBitmapContext) model' =
    let w = double context.Width
    let h = double context.Height
    let renderer, model = transformModel renderer' w h model'
    if renderer.Wireframe then
        renderWireframe (renderLine context) model
    else
        let render =
            if renderer.ZBufferEnabled then
                Array.fastFill renderer.ZBuffer System.Double.MaxValue
                if renderer.LightEnabled then renderTriangleZBufferPhong else renderTriangleZBuffer
            else
                if renderer.LightEnabled then renderTriangleAlwaysPhong else renderTriangleAlways
        renderFilled (render renderer context) model