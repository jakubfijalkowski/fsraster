module FsRaster.Filters

open FsRaster.CoreRendering

let minMaxColor (renderer : IRenderer) rect = 
    let app ((minR, minG, minB), (maxR, maxG, maxB)) c =
        let r = Colors.getR c
        let g = Colors.getG c
        let b = Colors.getB c
        let minimum = (min minR r, min minG g, min minB b)
        let maximum = (max maxR r, max maxG g, max maxB b)
        (minimum, maximum)
    renderer.Fold rect app ((255, 255, 255), (0, 0, 0))

let normalizeChannel min max c =
    let minF = double min
    let maxF = double max
    let cF = double c
    let value = (cF - minF) / (maxF - minF) * 255.0
    int (round value)

let normalizePixel (minR, minG, minB) (maxR, maxG, maxB) c =
    let r = normalizeChannel minR maxR (Colors.getR c)
    let g = normalizeChannel minG maxG (Colors.getG c)
    let b = normalizeChannel minB maxB (Colors.getB c)
    Colors.fromRGB r g b

let normalizeRenderer rect (renderer : CoreRendering.IRenderer) =
    let minC, maxC = minMaxColor renderer rect
    renderer.Map rect (normalizePixel minC maxC)
    ()

let generateHistogram bg channel pixels =
    let histogram = Array.zeroCreate 256
    pixels |> Array.filter (fun c -> c <> bg) |> Array.iter (fun pix ->
        let c = channel pix
        histogram.[c] <- histogram.[c] + 1
    )
    histogram