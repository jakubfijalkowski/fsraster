module FsRaster.CoreAlgorithms

open System.Collections.Generic

open FsRaster.Figures
open FsRaster.CoreRendering

let private boundaryFill bColor' newColor' pt (ctx : IRenderer) populate =
    let bColor = Colors.fromUIColor bColor'
    let newColor = Colors.fromUIColor newColor'
    let queue = new Queue<Point>()
    queue.Enqueue pt
    while queue.Count > 0 do
        let x, y = queue.Dequeue()
        if x >= 0 && y >= 0 && x < ctx.Width && y < ctx.Height then
            let c = ctx.GetPixel x y
            if c <> bColor && c <> newColor then
                ctx.PutPixel x y newColor
                populate x y |> List.iter queue.Enqueue

let boundaryFill4 bColor newColor x y (ctx : IRenderer) =
    boundaryFill bColor newColor (x, y) ctx (fun x y ->
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)])

let boundaryFill8 bColor newColor x y (ctx : IRenderer) =
    boundaryFill bColor newColor (x, y) ctx (fun x y ->
        [ for dx in [ -1 .. 1 ] do
          for dy in [ -1 .. 1 ] do
              yield (x + dx, y + dy)
        ])
