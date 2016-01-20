module FsRaster.D3.Renderer

open System.Diagnostics.CodeAnalysis
open FSharp.Collections.ParallelSeq

open FsRaster
open FsRaster.Utils
open FsRaster.D3.Math
open FsRaster.D3.Models
open FsRaster.D3.Camera

type Renderer3D =
    {
        Model : Matrix4;
        Projection : Matrix4;
        Cumulative : Matrix4;
        Camera : Camera;
        Wireframe : bool
    }

// Well, it's a point w/ XY-coordinates in screen space and Z coordinate in clip space
// This will hopefully allow to render everything and use Z-buffer
type Point3D = int * int * double

type RenderPrimitive =
    | PrimPoints of Point3D array
//                     | PrimLines of (Point3D * Point3D) array

[<Literal>]
let NearPlane = 0.1

[<Literal>]
let FarPlane = 100.0

let defaultRenderer =
    {
        Model = matIdentity;
        Projection = matIdentity;
        Cumulative = matIdentity;
        Camera = defaultCamera;
        Wireframe = true
    }

let setCameraTo camera renderer =
    let cam = updateMatrix camera
    let cumulative = renderer.Projection * cam.View * renderer.Model
    { renderer with Camera = cam; Cumulative = cumulative }

let updateProjection renderer width height =
    let aspect = double width / double height
    let newProj = matProjection 90.0 aspect NearPlane FarPlane
    let cumulative = newProj * renderer.Camera.View * renderer.Model
    { renderer with Projection = newProj; Cumulative = cumulative }

let inline toggleWireframe renderer =
    { renderer with Wireframe = not renderer.Wireframe }

let private toClipSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> (renderer.Cumulative * v).Normalized)
    { model with Vertices = newVerts }

let private toScreenSpace w h model =
    let newVerts = model.Vertices |> Array.map (fun v ->
        vec4 ((v.X + 1.0) / 2.0 * w) ((-v.Y + 1.0) / 2.0 * h) v.Z 1.0
    )
    { model with Vertices = newVerts }

let inline private isInView (v : Vector4) =
    v.X >= -1.0 && v.X <= 1.0 && v.Y >= -1.0 && v.Y <= 1.0 && v.Z >= -1.0 && v.Z <= 1.0

let private clipModel model =
    let newTris =
        model.Triangles
        |> Array.filter(fun (a, b, c) ->
            let va = model.Vertices.[a]
            let vb = model.Vertices.[b]
            let vc = model.Vertices.[c]
            isInView va && isInView vb && isInView vc
        )
    { model with Triangles = newTris }

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private renderLine (v1 : Vector4) (v2 : Vector4) =
    let x1' = int (round v1.X)
    let x2' = int (round v2.X)
    let y1' = int (round v1.Y)
    let y2' = int (round v2.Y)
    if x1' = x2' && y1' = y2' then Array.singleton (x1', y1', 0.0)
    else
        let renderOctant x1 y1 x2 y2 =
            let dx = x2 - x1
            let dy = y2 - y1
            let d' = 2 * dy - dx
            let incE = 2 * dy
            let incNE = 2 * (dy - dx)
            let rec build acc d x y =
                if x > x2 then acc
                else
                    let acc' = (x, y) :: acc
                    if d < 0
                    then build acc' (d + incE) (x + 1) y
                    else build acc' (d + incNE) (x + 1) (y + 1)
            build [] d' x1 y1
        let dx = x2' - x1'
        let dy = y2' - y1'
        let result =
            match (x2' - x1', y2' - y1') with
            | dx, dy when            dy >= 0 &&  dx >=  dy -> renderOctant  x1'  y1'  x2'  y2' |> List.map (fun (x, y) -> ( x,  y, 0.0))
            | dx, dy when dx >= 0 &&             dy >   dx -> renderOctant  y1'  x1'  y2'  x2' |> List.map (fun (x, y) -> ( y,  x, 0.0))
            | dx, dy when dx <  0 && dy >= 0 && -dx >=  dy -> renderOctant -x1'  y1' -x2'  y2' |> List.map (fun (x, y) -> (-x,  y, 0.0))
            | dx, dy when dx <  0 &&             dy >  -dx -> renderOctant  y1' -x1'  y2' -x2' |> List.map (fun (x, y) -> (-y,  x, 0.0))
            | dx, dy when dx <  0 && dy <  0 && -dx >= -dy -> renderOctant -x1' -y1' -x2' -y2' |> List.map (fun (x, y) -> (-x, -y, 0.0))
            | dx, dy when dx <  0 && dy <  0 && -dy >  -dx -> renderOctant -y1' -x1' -y2' -x2' |> List.map (fun (x, y) -> (-y, -x, 0.0))
            | dx, dy when dx >= 0 && dy <  0 && -dy >=  dx -> renderOctant -y1'  x1' -y2'  x2' |> List.map (fun (x, y) -> ( y, -x, 0.0))
            | dx, dy when dx >  0 && dy <  0 &&  dx >  -dy -> renderOctant  x1' -y1'  x2' -y2' |> List.map (fun (x, y) -> ( x, -y, 0.0))
            | _ -> []
        result |> List.toArray
    
let private renderWireframe model =
    let rec renderTriangles acc = function
        | ArrayCons ((a, b, c), rest) ->
            let v1 = model.Vertices.[a]
            let v2 = model.Vertices.[b]
            let v3 = model.Vertices.[c]
            let pl1 = renderLine v1 v2
            let pl2 = renderLine v1 v3
            let pl3 = renderLine v2 v3
            let acc' = PrimPoints pl1 :: PrimPoints pl2 :: PrimPoints pl3 :: acc
            renderTriangles acc' rest
        | ArrayNil -> acc
    renderTriangles [] (ConsArray.ofArray model.Triangles)

let renderModel renderer w h model =
    let conv = model |> toClipSpace renderer |> clipModel |> toScreenSpace w h
    // TODO: non-wireframe!
    conv |> renderWireframe