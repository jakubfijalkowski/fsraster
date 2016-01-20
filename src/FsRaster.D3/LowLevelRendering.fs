module FsRaster.D3.LowLevelRendering

open System.Diagnostics.CodeAnalysis

open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Renderer

#nowarn "9"

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private renderLine ctx (v1 : Vector4) (v2 : Vector4) c =
    let x1' = int v1.X
    let x2' = int v2.X
    let y1' = int v1.Y
    let y2' = int v2.Y
    if x1' = x2' && y1' = y2' then
        NativeInterop.NativePtr.set ctx.Context.Pixels (x1' + y1' * ctx.Width) c
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
                NativeInterop.NativePtr.set ctx.Context.Pixels (x' + y' * ctx.Width) c
                if d < 0 then build (d + incE) (x + 1) y
                else build (d + incNE) (x + 1) (y + 1)
        build d' x1 y1
        ()

// Scanline algorithm, but adjusted to triangles in 3D - should be faster than scanline from FsRaster.FigureRendering
let inline private sortVertices (v1 : Vector4) (v2 : Vector4) (v3 : Vector4) =
    let arr = [| v1; v2; v3 |]
    Array.sortInPlaceBy (fun v -> v.Y) arr
    (arr.[0], arr.[1], arr.[2])

type ActiveEdge = { YMax : int; mutable X : double; mutable Z : double; CoeffX : double; CoeffZ : double }

let inline private buildTopTriangle (v1 : Vector4) (v2 : Vector4) (v3 : Vector4) =
    let dy = double (v1.Y - v3.Y)
    let mx1 = double (v1.X - v3.X) / dy
    let mz1 = double (v1.Z - v3.Z) / dy
    let mx2 = double (v2.X - v3.X) / dy
    let mz2 = double (v2.Z - v3.Z) / dy
    let ae1 = { YMax = int v3.Y; X = v1.X; Z = v1.Z; CoeffX = mx1; CoeffZ = mz1 }
    let ae2 = { YMax = int v3.Y; X = v2.X; Z = v2.Z; CoeffX = mx2; CoeffZ = mz2 }
    (ae1, ae2)

let inline private buildBottomTriangle (v1 : Vector4) (v2 : Vector4) (v3 : Vector4) =
    let dy = double (v1.Y - v3.Y)
    let mx1 = double (v1.X - v3.X) / dy
    let mz1 = double (v1.Z - v3.Z) / dy
    let mx2 = double (v1.X - v2.X) / dy
    let mz2 = double (v1.Z - v2.Z) / dy
    let ae1 = { YMax = int v3.Y; X = v1.X; Z = v1.Z; CoeffX = mx1; CoeffZ = mz1 }
    let ae2 = { YMax = int v3.Y; X = v1.X; Z = v1.Z; CoeffX = mx2; CoeffZ = mz2 }
    (ae1, ae2)

let inline private buildProperTriangle (v1 : Vector4) (v2 : Vector4) (v3 : Vector4) =
    let dy13 = double (v1.Y - v3.Y)
    let dy12 = double (v1.Y - v2.Y)
    let dy23 = double (v2.Y - v3.Y)
    let mx1 = double (v1.X - v3.X) / dy13
    let mz1 = double (v1.Z - v3.Z) / dy13
    let mx2 = double (v1.X - v2.X) / dy12
    let mz2 = double (v1.Z - v2.Z) / dy12
    let mx3 = double (v2.X - v3.X) / dy23
    let mz3 = double (v2.Z - v3.Z) / dy23

    let midX = v1.X - mx1 * dy12

    let ae1 = { YMax = int v2.Y; X = v1.X; Z = v1.Z; CoeffX = mx1; CoeffZ = mz1 }
    let ae2 = { YMax = int v2.Y; X = v1.X; Z = v1.Z; CoeffX = mx2; CoeffZ = mz2 }

    let ae3 = { YMax = int v3.Y; X = midX; Z = v1.Z; CoeffX = mx1; CoeffZ = mz1 }
    let ae4 = { YMax = int v3.Y; X = v2.X; Z = v2.Z; CoeffX = mx3; CoeffZ = mz3 }
    [ ae1, ae2; ae3, ae4 ]

let private renderScanlineAlways ctx c ymin (ae1, ae2) =
    let ymax = ae1.YMax
    for y = ymin to ymax - 1 do
        let x1 = int <| round ae1.X
        let x2 = int <| round ae2.X
        let minX = min x1 x2
        let maxX = max x1 x2
        for x = minX to maxX do
            NativeInterop.NativePtr.set ctx.Context.Pixels (x + y * ctx.Width) c
        ae1.X <- ae1.X + ae1.CoeffX
        ae2.X <- ae2.X + ae2.CoeffX
    ymax

let private renderScanlineZBuffer (zbuffer : double array) ctx c ymin (ae1, ae2) =
    let ymax = ae1.YMax
    for y = ymin to ymax - 1 do
        let x1 = int <| round ae1.X
        let x2 = int <| round ae2.X
        let minX = min x1 x2
        let maxX = max x1 x2

        let mz = (ae2.Z - ae1.Z) / (ae2.X - ae1.X) 
        let mutable z = ae1.Z
        for x = minX to maxX do
            let idx = y * ctx.Context.Width + x
            if zbuffer.[idx] > z then
                NativeInterop.NativePtr.set ctx.Context.Pixels idx c
                zbuffer.[idx] <- z
            z <- z + mz
        ae1.X <- ae1.X + ae1.CoeffX
        ae1.Z <- ae1.Z + ae1.CoeffZ
        ae2.X <- ae2.X + ae2.CoeffX
        ae2.Z <- ae2.Z + ae2.CoeffZ
    ymax

let private renderTriangle ctx render (v1' : Vector4) (v2' : Vector4) (v3' : Vector4) c =
    let v1, v2, v3 = sortVertices v1' v2' v3'
    let ymin = int v1.Y
    let aes =
        if v1.Y = v2.Y then [ buildTopTriangle v1 v2 v3 ]
        else if v2.Y = v3.Y then [ buildBottomTriangle v1 v2 v3 ]
        else buildProperTriangle v1 v2 v3
    aes |> List.fold (render ctx c) ymin |> ignore

let drawModel renderer context model =
    let w = double context.Width
    let h = double context.Height
    let transformed = transformModel renderer w h model
    if renderer.Wireframe then
        renderWireframe (renderLine context) transformed
    else
        let render =
            if renderer.ZBuffer then
                let zBuffer = Array.create (context.Width * context.Height) System.Double.MaxValue
                renderScanlineZBuffer zBuffer
            else
                renderScanlineAlways
        renderFilled (renderTriangle context render) transformed
    ()