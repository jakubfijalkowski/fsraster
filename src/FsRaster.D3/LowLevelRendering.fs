module FsRaster.D3.LowLevelRendering

open System.Diagnostics.CodeAnalysis

open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Renderer

#nowarn "9"

let inline putPixel' ctx x y c =
    NativeInterop.NativePtr.set ctx.Context.Pixels (x + y * ctx.Width) c

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private renderLine' ctx (v1 : Vector4) (v2 : Vector4) =
    let x1' = int (floor v1.X)
    let x2' = int (floor v2.X)
    let y1' = int (floor v1.Y)
    let y2' = int (floor v2.Y)
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

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private renderLine ctx (v1 : Vector4) (v2 : Vector4) c =
    let x1' = int (floor v1.X)
    let x2' = int (floor v2.X)
    let y1' = int (floor v1.Y)
    let y2' = int (floor v2.Y)
    if x1' = x2' && y1' = y2' then putPixel' ctx x1' y1' c
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
                putPixel' ctx x' y' c
                if d < 0 then build (d + incE) (x + 1) y
                else build (d + incNE) (x + 1) (y + 1)
        build d' x1 y1
        ()

let drawModel renderer context model =
    let w = double context.Width
    let h = double context.Height
    let transformed = transformModel renderer w h model
    if renderer.Wireframe then
        renderWireframe (renderLine context) transformed
    ()