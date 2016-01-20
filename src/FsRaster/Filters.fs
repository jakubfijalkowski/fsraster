module FsRaster.Filters

open System.Diagnostics.CodeAnalysis

open System.Windows.Media
open System.Windows.Media.Imaging

open FsRaster.RawRendering
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

let generateHistogram channel ctx rect =
    let w = ctx.Width
    let h = ctx.Height
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let pixels = ctx.Context.Pixels
    let histogram = Array.zeroCreate 256
    for y in [ top .. bottom ] do
        for x in [ left .. right ] do
            let idx = y * w + x
            let pix = NativeInterop.NativePtr.get pixels idx
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

[<SuppressMessage("CyclomaticComplexity", "*")>]
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

let private prepareWorkImage (left, top, right, bottom) =
    let w = right - left
    let h = bottom - top
    let size = (max w h) * 2
    let bmp = BitmapFactory.New(size, size)
    { Width = size; Height = size; Context = bmp.GetBitmapContext(ReadWriteMode.ReadWrite) }

let private copyBetween src dst (left, top, right, bottom) (offsetX, offsetY) =
    let srcStride = src.Width
    let dstStride = dst.Width
    let w = right - left
    let h = bottom - top
    let srcPixels = src.Context.Pixels
    let dstPixels = dst.Context.Pixels
    for y in 0 .. h do
        for x in 0 .. w do
            let srcIdx = (top + y) * srcStride + left + x
            let dstIdx = (offsetY + y) * dstStride + offsetX + x
            let pix = NativeInterop.NativePtr.get srcPixels srcIdx
            NativeInterop.NativePtr.set dstPixels dstIdx pix

let private getWorkRect (fromLeft, fromTop, fromRight, fromBottom) toCtx =
    let realW = fromRight - fromLeft
    let realH = fromBottom - fromTop
    let cx = toCtx.Width / 2
    let cy = toCtx.Height / 2
    let offsetX = cx - realW / 2
    let offsetY = cy - realH / 2
    (offsetX, offsetY, offsetX + realW, offsetY + realH)

let private getWorkRectRot (fromLeft, fromTop, fromRight, fromBottom) toCtx =
    let realW = fromRight - fromLeft
    let realH = fromBottom - fromTop
    let cx = toCtx.Width / 2
    let cy = toCtx.Height / 2
    let offsetX = cx - realH / 2
    let offsetY = cy - realW / 2
    (offsetX, offsetY, offsetX + realH, offsetY + realW)

let private rotate0 fromCtx ((fromLeft, fromTop, fromRight, fromBottom) as fromRect) toCtx =
    let workRect = getWorkRect fromRect toCtx
    let offsetX, offsetY, _, _ = workRect
    copyBetween fromCtx toCtx fromRect (offsetX, offsetY)
    workRect

let private rotate90 fromCtx ((fromLeft, fromTop, fromRight, fromBottom) as fromRect) toCtx =
    let srcStride = fromCtx.Width
    let dstStride = toCtx.Width
    let workRect = getWorkRectRot fromRect toCtx
    let offsetX, offsetY, dstRight, dstBottom = workRect
    let w = fromRight - fromLeft
    let h = fromBottom - fromTop

    let srcPixels = fromCtx.Context.Pixels
    let dstPixels = toCtx.Context.Pixels

    for y in 0 .. h do
        for x in 0 .. w do
            let srcIdx = (fromTop + y) * srcStride + fromLeft + x
            let dstIdx = (offsetY + x) * dstStride + dstRight - y
            NativeInterop.NativePtr.get srcPixels srcIdx
            |> NativeInterop.NativePtr.set dstPixels dstIdx
    workRect

let private rotate180 fromCtx ((fromLeft, fromTop, fromRight, fromBottom) as fromRect) toCtx =
    let srcStride = fromCtx.Width
    let dstStride = toCtx.Width
    let workRect = getWorkRect fromRect toCtx
    let offsetX, offsetY, dstRight, dstBottom = workRect
    let w = fromRight - fromLeft
    let h = fromBottom - fromTop

    let srcPixels = fromCtx.Context.Pixels
    let dstPixels = toCtx.Context.Pixels

    for y in 0 .. h do
        for x in 0 .. w do
            let srcIdx = (fromTop + y) * srcStride + fromLeft + x
            let dstIdx = (dstBottom - y) * dstStride + dstRight - x
            NativeInterop.NativePtr.get srcPixels srcIdx
            |> NativeInterop.NativePtr.set dstPixels dstIdx
    
    workRect

let private rotate270 fromCtx ((fromLeft, fromTop, fromRight, fromBottom) as fromRect) toCtx =
    let srcStride = fromCtx.Width
    let dstStride = toCtx.Width
    let workRect = getWorkRectRot fromRect toCtx
    let offsetX, offsetY, dstRight, dstBottom = workRect
    let w = fromRight - fromLeft
    let h = fromBottom - fromTop

    let srcPixels = fromCtx.Context.Pixels
    let dstPixels = toCtx.Context.Pixels

    for y in 0 .. h do
        for x in 0 .. w do
            let srcIdx = (fromTop + y) * srcStride + fromLeft + x
            let dstIdx = (dstBottom - x) * dstStride + offsetX + y
            NativeInterop.NativePtr.get srcPixels srcIdx
            |> NativeInterop.NativePtr.set dstPixels dstIdx
    workRect

let private performBaseRotations srcCtx srcRect dstCtx angle =
    if angle > 45.0 && angle <= 135.0 then
        (rotate90 srcCtx srcRect dstCtx, angle - 90.0)
    else if angle > 135.0 && angle <= 225.0 then
        (rotate180 srcCtx srcRect dstCtx, angle - 180.0)
    else if angle > 225.0 && angle <= 315.0 then
        (rotate270 srcCtx srcRect dstCtx, angle - 270.0)
    else
        (rotate0 srcCtx srcRect dstCtx, angle)

let inline private toTriple pix =
    let r = Colors.getR pix
    let g = Colors.getG pix
    let b = Colors.getB pix
    (r, g, b)

let inline private (%+%) (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)
let inline private (%-%) (r1, g1, b1) (r2, g2, b2) = (r1 - r2, g1 - g2, b1 - b2)
let inline private (%*%) (r, g, b) factor =
    (int <| double r * factor, int <| double g * factor, int <| double b * factor)

let inline private tripleToRGB (r, g, b) =
    Colors.fromRGB (Colors.clamp r) (Colors.clamp g) (Colors.clamp b)
    
[<SuppressMessage("CyclomaticComplexity", "*")>]
let private shearX ctx shear (left, top, right, bottom) =
    let stride = ctx.Width
    let pixels = ctx.Context.Pixels
    let h = bottom - top
    let w = right - left
    let hTop = h / 2
    let hBottom = h - hTop
    let mutable minSkew = 9999
    let mutable maxSkew = 0
    for baseY in -hTop .. hBottom do
        let y = top + baseY + hTop
        let skew = shear * (double baseY + 0.5)
        let skewi = int <| floor skew
        let skewf = if skew < 0.0 then skew - double skewi else double skewi - skew
        minSkew <- min minSkew skewi
        maxSkew <- max maxSkew skewi
        let mutable prevPixel = (0, 0, 0)
        for baseX in 0 .. w do
            let x = if skew >= 0.0 then right - baseX else left + baseX
            let srcIdx = y * stride + x
            let pix' = NativeInterop.NativePtr.get pixels srcIdx |> toTriple
            let leftPix = pix' %*% skewf
            let pix = pix' %-% (leftPix %-% prevPixel)
            prevPixel <- leftPix

            if x + skewi >= 0 && x + skewi < ctx.Width then
                let dstIdx = srcIdx + skewi
                NativeInterop.NativePtr.set pixels dstIdx (tripleToRGB pix)

        if skewi > 0 then
            for x in 0 .. skewi - 1 do
                NativeInterop.NativePtr.set pixels (y * stride + x + left) 0xff000000
        else if skewi < 0 then
            for x in right + skewi + 1 .. right do
                NativeInterop.NativePtr.set pixels (y * stride + x) 0xff000000
    (left + minSkew, top, right + maxSkew, bottom)

[<SuppressMessage("CyclomaticComplexity", "*")>]
let private shearY ctx shear (left, top, right, bottom) =
    let stride = ctx.Width
    let pixels = ctx.Context.Pixels
    let h = bottom - top
    let w = right - left
    let wLeft = w / 2
    let wRight = w - wLeft
    let mutable minSkew = 9999
    let mutable maxSkew = 0
    for baseX in -wLeft .. wRight do
        let x = left + baseX + wLeft
        let skew = shear * (double baseX + 0.5)
        let skewi = int <| floor skew
        let skewf = if skew < 0.0 then skew - double skewi else double skewi - skew
        minSkew <- min minSkew skewi
        maxSkew <- max maxSkew skewi
        let mutable prevPixel = (0, 0, 0)
        for baseY in 0 .. h do
            let y = if skew >= 0.0 then bottom - baseY else top + baseY 
            let srcIdx = y * stride + x
            let pix' = NativeInterop.NativePtr.get pixels srcIdx |> toTriple
            let leftPix = pix' %*% skewf
            let pix = pix' %-% (leftPix %-% prevPixel)
            prevPixel <- leftPix

            if y + skewi >= 0 && y + skewi < ctx.Height then
                let dstIdx = (y + skewi) * stride + x
                NativeInterop.NativePtr.set pixels dstIdx (tripleToRGB pix)

        if skewi > 0 then
            for y in 0 .. skewi - 1 do
                NativeInterop.NativePtr.set pixels ((y + top) * stride + x) 0xff000000
        else if skewi < 0 then
            for y in bottom + skewi + 1 .. bottom do
                NativeInterop.NativePtr.set pixels (y * stride + x) 0xff000000
    (left, top + minSkew, right, bottom + maxSkew)

let rotateImage ctx angle rect =
    let w = ctx.Width
    let h = ctx.Height
    let inputRect = FsRaster.Figures.clipRect rect (w - 1) (h - 1)
    let left, top, right, bottom = inputRect

    let workImage = prepareWorkImage inputRect
    let workRect = getWorkRect inputRect workImage
    clearBitmap workImage 0xff000000

    let (newRect, angle') = performBaseRotations ctx inputRect workImage angle

    let angleRad = angle' * System.Math.PI / 180.0
    let alpha = - tan (angleRad / 2.0)
    let beta = sin angleRad

    let shearRect1 = shearX workImage alpha newRect
    let shearRect2 = shearY workImage beta shearRect1
    shearX workImage alpha shearRect2 |> ignore

    copyBetween workImage ctx workRect (left, top)
    workImage.Context.Dispose()
    System.GC.Collect()

let private gammaCorrectPix gamma pix =
    let r = double (Colors.getR pix) / 255.0
    let g = double (Colors.getG pix) / 255.0
    let b = double (Colors.getB pix) / 255.0
    let newR = (r ** gamma) * 255.0
    let newG = (g ** gamma) * 255.0
    let newB = (b ** gamma) * 255.0
    Colors.fromRGB (int newR) (int newG) (int newB)
        
let gammaCorrect ctx gamma rect =
    let stride = ctx.Width
    let left, top, right, bottom = FsRaster.Figures.clipRect rect (ctx.Width - 1) (ctx.Height - 1)
    
    let pixels = ctx.Context.Pixels
    for y in top .. bottom do
        for x in left .. right do
            let idx = y * stride + x
            NativeInterop.NativePtr.get pixels idx
            |> gammaCorrectPix gamma
            |> NativeInterop.NativePtr.set pixels idx