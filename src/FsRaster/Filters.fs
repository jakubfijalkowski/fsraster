module FsRaster.Filters

open System.Diagnostics.CodeAnalysis

open FsRaster.CoreRendering

#nowarn "9"

let minMaxColor ctx w (left, top, right, bottom) = 
    let pixels = ctx.Context.Pixels
    let mutable minR, minG, minB = 255, 255, 255
    let mutable maxR, maxG, maxB = (0, 0, 0)
    for y in [ top .. bottom ] do
        for x in [ left .. right ] do
            let idx = y * w + x
            let pix = NativeInterop.NativePtr.get pixels idx
            let r = Colors.getR pix
            let g = Colors.getG pix
            let b = Colors.getB pix
            minR <- min minR r
            minG <- min minG g
            minB <- min minB b
            maxR <- max maxR r
            maxG <- max maxG g
            maxB <- max maxB b
    ((minR, minG, minB), (maxR, maxG, maxB))


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

let normalizeHistogram ctx rect =
    let w = ctx.Width
    let h = ctx.Height
    let pixels = ctx.Context.Pixels
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let minC, maxC = minMaxColor ctx w (left, top, right, bottom)
    for y in [ top .. bottom ] do
        for x in [ left .. right ] do
            let idx = y * w + x
            let pix = NativeInterop.NativePtr.get pixels idx
            let newPix = normalizePixel minC maxC pix
            NativeInterop.NativePtr.set pixels idx newPix

let generateHistogram bg channel ctx rect =
    let w = ctx.Width
    let h = ctx.Height
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let pixels = ctx.Context.Pixels
    let histogram = Array.zeroCreate 256
    for y in [ top .. bottom ] do
        for x in [ left .. right ] do
            let idx = y * w + x
            let pix = NativeInterop.NativePtr.get pixels idx
            if pix <> bg then
                let c = channel pix
                histogram.[c] <- histogram.[c] + 1
    histogram

[<SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let convolvePixel size (matrix : double array) (copy : int array) w h x y =
    let hs = size / 2
    let mutable sumR, sumG, sumB = 0.0, 0.0, 0.0
    for dy in -hs .. hs do
        for dx in -hs .. hs do
            let x' = x + dx
            let y' = y + dy
            if x' >= 0 && y' >= 0 && x' < w && y' < h then
                let weight = matrix.[(dy + hs) * size + dx + hs]
                let idx = y' * w + x'
                let pix = copy.[idx]
                let r = Colors.getR pix
                let g = Colors.getG pix
                let b = Colors.getB pix
                sumR <- sumR + double r * weight
                sumG <- sumG + double g * weight
                sumB <- sumB + double b * weight
    (sumR, sumG, sumB)

[<SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let convolve ctx size matrix offset coeff rect =
    let w = ctx.Width
    let h = ctx.Height
    let pixels = ctx.Context.Pixels
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let rectW = right - left
    let rectH = bottom - top
    let pixelsCopy = streamPixels ctx rect
    for y in 0 .. rectH do
        for x in 0 .. rectW do
            let r, g, b = convolvePixel size matrix pixelsCopy rectW rectH x y
            let r' = r / coeff + offset
            let g' = g / coeff + offset
            let b' = b / coeff + offset
            let pix = Colors.fromRGB (Colors.clamp <| int r') (Colors.clamp <| int g') (Colors.clamp <| int b')
            let idx = (y + top) * w + x + right
            NativeInterop.NativePtr.set pixels idx pix
