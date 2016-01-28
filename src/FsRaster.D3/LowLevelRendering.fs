module FsRaster.D3.LowLevelRendering

open System.Diagnostics.CodeAnalysis

open FsRaster
open FsRaster.Utils
open FsRaster.RawRendering
open FsRaster.D3
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
    let pixels = ctx.Pixels
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

let private renderFilled renderer (context : PixelBufferContext) model =
    let tris = model.Triangles |> Array.map (Native.toRenderTriangle model)
    Native.pinAndRender renderer.Width renderer.Height renderer.ZBufferEnabled context.Pixels tris

let drawModel renderer (context : PixelBufferContext) model' =
    let w = double context.Width
    let h = double context.Height
    let model = transformModel renderer w h model'
    if renderer.Wireframe then
        renderWireframe (renderLine context) model
    else
        renderFilled renderer context model