module FsRaster.CoreAlgorithms

open FsRaster.CoreRendering

let boundaryFill4 bColor' newColor' x y (ctx : IRenderer) =
    let bColor = convertColor bColor'
    let newColor = convertColor newColor'
    let rec boundaryFill4' x y =
        if x >= 0 && y >= 0 && x < ctx.Width && y < ctx.Width then
            let c = ctx.GetPixel x y
            if c <> bColor && c <> newColor then
                ctx.PutPixel x y newColor
                boundaryFill4' x (y - 1)
                boundaryFill4' x (y + 1)
                boundaryFill4' (x - 1) y
                boundaryFill4' (x + 1) y
    boundaryFill4' x y

let boundaryFill8 bColor' newColor' x y (ctx : IRenderer) =
    let bColor = convertColor bColor'
    let newColor = convertColor newColor'
    let rec boundaryFill8' x y =
        if x >= 0 && y >= 0 && x < ctx.Width && y < ctx.Width then
            let c = ctx.GetPixel x y
            if c <> bColor && c <> newColor then
                ctx.PutPixel x y newColor
                boundaryFill8' (x - 1) (y - 1)
                boundaryFill8'  x      (y - 1)
                boundaryFill8' (x + 1) (y - 1)
                boundaryFill8' (x - 1)  y
                boundaryFill8'  x       y
                boundaryFill8' (x + 1)  y
                boundaryFill8' (x - 1) (y + 1)
                boundaryFill8'  x      (y + 1)
                boundaryFill8' (x + 1) (y + 1)
    boundaryFill8' x y
