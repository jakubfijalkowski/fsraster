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
            let x' = max 0 (min (w - 1) (x + dx))
            let y' = max 0 (min (h - 1) (y + dy))
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
    let pixelsCopy = streamPixels ctx (left, top, right, bottom)
    let rectW = right - left
    let rectH = bottom - top
    for y in 0 .. rectH do
        for x in 0 .. rectW do
            let r, g, b = convolvePixel size matrix pixelsCopy rectW rectH x y
            let r' = r / coeff + offset
            let g' = g / coeff + offset
            let b' = b / coeff + offset
            let pix = Colors.fromRGB (Colors.clamp <| int r') (Colors.clamp <| int g') (Colors.clamp <| int b')
            let idx = (y + top) * w + x + left
            NativeInterop.NativePtr.set pixels idx pix

let applyFunctionFilter ctx rect (filterR : int array) (filterG : int array) (filterB : int array) =
    let w = ctx.Width
    let h = ctx.Height
    let pixels = ctx.Context.Pixels
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    for y in top .. bottom do
        for x in left .. right do
            let idx = y * w + x
            let pix = NativeInterop.NativePtr.get pixels idx
            let r = filterR.[Colors.getR pix]
            let g = filterG.[Colors.getG pix]
            let b = filterB.[Colors.getB pix]
            NativeInterop.NativePtr.set pixels idx (Colors.fromRGB r g b)

let scaleImage ctx scaleX scaleY rect =
    let w = ctx.Width
    let h = ctx.Height
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let pixelsCopy = streamPixels ctx (left, top, right + 1, bottom + 1)

    let invScaleX = 1.0 / scaleX
    let invScaleY = 1.0 / scaleY

    let srcW = right - left + 1
    let srcH = bottom - top + 1
    let dstW = int <| double srcW * scaleX
    let dstH = int <| double srcH * scaleY

    let leftOffset = max 0 (srcW - dstW) / 2
    let rightOffset = max 0 (srcW - leftOffset - dstW)
    let topOffset = max 0 (srcH - dstH) / 2
    let bottomOffset = max 0 (srcH - topOffset - dstH)

    let pixels = ctx.Context.Pixels
    // Black background
    //  Top
    for y in top .. top + topOffset - 1 do
        for idx in y * w + left .. y * w + right - 1 do
            NativeInterop.NativePtr.set pixels idx 0xff000000
    //  Bottom
    for y in bottom - bottomOffset + 1 .. bottom do
        for idx in y * w + left .. y * w + right - 1 do
            NativeInterop.NativePtr.set pixels idx 0xff000000
    //  Left
    for x in left .. left + leftOffset - 1 do
        for idx in x + top * w .. w .. x + bottom * w do
            NativeInterop.NativePtr.set pixels idx 0xff000000
    //  Right
    for x in right - rightOffset + 1 .. right do
        for idx in x + top * w .. w .. x + bottom * w do
            NativeInterop.NativePtr.set pixels idx 0xff000000

    let srcCX = srcW / 2
    let srcCY = srcH / 2
    let dstCX = left + leftOffset + dstW / 2 - if scaleX > 1.0 then (dstW - srcW) / 2 else 0
    let dstCY = top + topOffset + dstH / 2 - if scaleY > 1.0 then (dstH - srcH) / 2 else 0

    //  Real image
    for y in top + topOffset .. bottom - bottomOffset do
        for x in left + leftOffset .. right - rightOffset do
            let distX = x - dstCX
            let distY = y - dstCY
            let srcX = srcCX + (int <| double distX * invScaleX)
            let srcY = srcCY + (int <| double distY * invScaleY)
            let dstIdx = y * w + x
            let srcIdx = srcY * srcW + srcX
            NativeInterop.NativePtr.set pixels dstIdx pixelsCopy.[srcIdx]