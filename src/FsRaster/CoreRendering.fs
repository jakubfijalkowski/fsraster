module FsRaster.CoreRendering

open System
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Diagnostics.CodeAnalysis

open FSharp.Collections.ParallelSeq

open FsRaster.Figures
open FsRaster.FigureRendering

// Required to fight thread access exceptions
type CachedBitmapContext = {
    Context : BitmapContext;
    Width : int;
    Height : int
}

#nowarn "9"

// https://github.com/teichgraf/WriteableBitmapEx/blob/master/Source/WriteableBitmapEx/WriteableBitmapBaseExtensions.cs#L79-L105
let clearBitmap ctx c =
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let len = w * 4 //SizeOfArgb

    for x = 0 to w do
        NativeInterop.NativePtr.set pixels x c

    let mutable blockHeight = 1
    let mutable y = 1
    while y < h do
        BitmapContext.BlockCopy(ctx.Context, 0, ctx.Context, y * len, blockHeight * len)
        y <- y + blockHeight
        blockHeight <- min (2 * blockHeight) (h - y)

// http://stackoverflow.com/questions/12011081/alpha-blending-2-rgba-colors-in-c
[<SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let blendPixels r1 g1 b1 r2 g2 b2 a' =
    let a = a' + 1
    let inv_a = 256 - a'
    let r' = (a * r2 + inv_a * r1) >>> 8
    let g' = (a * g2 + inv_a * g1) >>> 8
    let b' = (a * b2 + inv_a * b1) >>> 8
    Colors.fromRGB r' g' b'

// https://github.com/teichgraf/WriteableBitmapEx/blob/master/Source/WriteableBitmapEx/WriteableBitmapBaseExtensions.cs#L392-L398
let putPixel ctx x y c =
    if x >= 0 && y >= 0 && x < ctx.Width && y < ctx.Height then
        let pixels = ctx.Context.Pixels
        let index = y * ctx.Width + x
        NativeInterop.NativePtr.set pixels index c

let putColorAlpha ctx x y c =
    if x >= 0 && y >= 0 && x < ctx.Width && y < ctx.Height then
        let index = y * ctx.Width + x
        let pixels = ctx.Context.Pixels
        let color = NativeInterop.NativePtr.get pixels index : int
        let b1 = Colors.getB color
        let g1 = Colors.getG color
        let r1 = Colors.getR color
        let b2 = Colors.getB c
        let g2 = Colors.getG c
        let r2 = Colors.getR c
        let a = Colors.getA c
        NativeInterop.NativePtr.set pixels index (blendPixels r1 g1 b1 r2 g2 b2 a)

let putSolidLine ctx x1' y x2' c =
    let initialBlockSize = 32
    let x1 = max x1' 0
    let x2 = min x2' (ctx.Width - 1)
    let w = x2 - x1

    if x2 >= 0 && x1 < ctx.Width && y >= 0 && y < ctx.Height && w >= 0 then
        // Here, we operate on ints (as pixels is nativeptr<int>)
        let startIdx = y * ctx.Width + x1
        let pixels = ctx.Context.Pixels
        for i in [ 0 .. min w (initialBlockSize - 1) ] do
            NativeInterop.NativePtr.set pixels (startIdx + i) c

        // But here, we must switch to bytes, as BlockCopy operates on bytes, not on ints
        let startIdx' = startIdx * 4
        let endIndex = (y * ctx.Width + x2) * 4
        let mutable index = (startIdx + initialBlockSize) * 4
        let mutable block = initialBlockSize * 4
        while index <= endIndex do
            BitmapContext.BlockCopy(ctx.Context, startIdx', ctx.Context, index, min block (endIndex - index + 4))
            index <- index + block
            block <- block * 2

// This is MUCH faster than rendering pixels in renderFilledPolygonTextured and then passing array of PrimPixels
// to the core renderer. Don't know why, but this way it renders hundreds of thousands of pixels in a bunch of
// miliseconds, not hundreds of miliseconds. Even when using good ol' mutable arrays it is waaaay slower than this.
let putTexLine ctx prim =
    let y = prim.Y
    let x1 = max prim.X1 0
    let x2 = min prim.X2 (ctx.Width - 1)
    let w = x2 - x1

    if x2 >= 0 && x1 < ctx.Width && y >= 0 && y < ctx.Height && w >= 0 then
        let startIdx = y * ctx.Width
        let pixels = ctx.Context.Pixels
        let originX, originY = prim.Origin
        let texY = abs ((originY - y) % prim.Texture.Height)
        for x in x1 .. x2 do
            let texX = abs (x - originX) % prim.Texture.Width
            let c = FigureColor.getTexPixel prim.Texture texX texY
            NativeInterop.NativePtr.set pixels (startIdx + x) c

let getPixel ctx x y =
    let index = y * ctx.Width + x
    let pixels = ctx.Context.Pixels
    NativeInterop.NativePtr.get pixels index

let putPrimitive ctx = function
    | PrimPixel ((x, y), c)   -> putColorAlpha ctx x y c
    | PrimLine (x1, y, x2, c) -> putSolidLine ctx x1 y x2 c
    | PrimTexLine t           -> putTexLine ctx t
    
type YUVPlane = PlaneY | PlaneU | PlaneV

let getYUVFromPixel p =
    let r = (double <| Colors.getR p) / 255.0
    let g = (double <| Colors.getG p) / 255.0
    let b = (double <| Colors.getB p) / 255.0
    let y = 0.299 * r + 0.587 * g + 0.114 * b
    let u = -0.147 * r - 0.289 * g + 0.436 * g
    let v = 0.615 * r - 0.515 * g - 0.1 * b
    (y, u, v)

let getPlane plane (y, u, v) =
    match plane with
    | PlaneY -> (y, 0.0, 0.0)
    | PlaneU -> (0.0, u, 0.0)
    | PlaneV -> (0.0, 0.0, v)

let yuvToRGB (y, u, v) =
    let r = (y + 1.140 * v) * 255.0
    let g = (y - 0.395 * u - 0.581 * v) * 255.0
    let b = (y + 2.032 * u) * 255.0
    Colors.fromRGB (int r) (int g) (int b)

let extractYUVPlane ctx plane =
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let len = w * h - 1
    for i in 0 .. len do
        let pix = NativeInterop.NativePtr.get pixels i
        let yuv = getYUVFromPixel pix |> getPlane plane
        let rgb = yuvToRGB yuv
        NativeInterop.NativePtr.set pixels i rgb

type IRenderer =
    inherit IDisposable

    abstract member Width : int
    abstract member Height : int

    abstract member Clear : Color -> unit
    abstract member PutColor : int -> int -> Color -> unit

    abstract member GetPixel : int -> int -> int
    abstract member PutPixel : int -> int -> int -> unit

    abstract member PutPrimitive : RenderPrimitive -> unit

    abstract member ExtractYUVPlane : YUVPlane -> unit
       
type BitmapRenderer(context : BitmapContext) =
    let cachedContext = { Context = context; Width = context.Width; Height = context.Height }

    interface IRenderer with
        member this.Width = cachedContext.Width
        member this.Height = cachedContext.Height

        member this.Clear color = clearBitmap cachedContext (Colors.fromUIColor color)
        member this.PutColor x y color = putColorAlpha cachedContext x y (Colors.fromUIColor color)

        member this.GetPixel x y = getPixel cachedContext x y
        member this.PutPixel x y c = putPixel cachedContext x y c

        member this.PutPrimitive prim = putPrimitive cachedContext prim

        member this.ExtractYUVPlane plane = extractYUVPlane cachedContext plane

    interface IDisposable with
        member x.Dispose() = context.Dispose()

let renderFigures (renderer : IRenderer) figures =
    let pixels = renderFigureList figures
    pixels |> List.iter (PSeq.iter renderer.PutPrimitive)