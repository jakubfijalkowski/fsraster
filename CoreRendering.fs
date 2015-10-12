module FsRaster.CoreRendering

open System
open System.Windows.Media
open System.Windows.Media.Imaging

open FSharp.Collections.ParallelSeq

open FsRaster.Figures

// Required to fight thread access exceptions
type CachedBitmapContext = {
    Context : BitmapContext;
    Width : int;
    Height : int
}

#nowarn "9"
// https://github.com/teichgraf/WriteableBitmapEx/blob/master/Source/WriteableBitmapEx/WriteableBitmapBaseExtensions.cs#L79-L105
let clearBitmap ctx c =
    let color = WriteableBitmapExtensions.ConvertColor(c)
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let len = w * 4 //SizeOfArgb

    for x = 0 to w do
        NativeInterop.NativePtr.set pixels x color

    let mutable blockHeight = 1
    let mutable y = 1
    while y < h do
        BitmapContext.BlockCopy(ctx.Context, 0, ctx.Context, y * len, blockHeight * len)
        y <- y + blockHeight
        blockHeight <- min (2 * blockHeight) (h - y)
        
// https://github.com/teichgraf/WriteableBitmapEx/blob/master/Source/WriteableBitmapEx/WriteableBitmapBaseExtensions.cs#L392-L398
let putPixel ctx x y c =
    let color = WriteableBitmapExtensions.ConvertColor(c)
    let pixels = ctx.Context.Pixels
    let index = y * ctx.Width + x
    NativeInterop.NativePtr.set pixels index color

// http://stackoverflow.com/questions/12011081/alpha-blending-2-rgba-colors-in-c
let blendPixels r1 g1 b1 r2 g2 b2 a' =
    let a = a' + 1
    let inv_a = 256 - a'
    let r' = (a * r2 + inv_a * r1) >>> 8
    let g' = (a * g2 + inv_a * g1) >>> 8
    let b' = (a * b2 + inv_a * b1) >>> 8
    (0xff <<< 24) ||| (r' <<< 16) ||| (g' <<< 8) ||| b'

let putPixelAlpha ctx x y (c : Color) =
    let index = y * ctx.Width + x
    let pixels = ctx.Context.Pixels
    let color = NativeInterop.NativePtr.get pixels index : int
    let b1 = color &&& 0xff
    let g1 = (color >>> 8) &&& 0xff
    let r1 = (color >>> 16) &&& 0xff
    let b2 = int c.B
    let g2 = int c.G
    let r2 = int c.R
    let a = int c.A
    NativeInterop.NativePtr.set pixels index (blendPixels r1 g1 b1 r2 g2 b2 a)

type IRenderer =
    inherit IDisposable

    abstract member Width : int
    abstract member Height : int

    abstract member Clear : Color -> unit
    abstract member PutPixel : int -> int -> Color -> unit
       
type BitmapRenderer(context : BitmapContext) =
    let cachedContext = { Context = context; Width = context.Width; Height = context.Height }

    interface IRenderer with
        member this.Width = cachedContext.Width
        member this.Height = cachedContext.Height

        member this.Clear color = clearBitmap cachedContext color
        member this.PutPixel x y color = putPixelAlpha cachedContext x y color

    interface IDisposable with
        member x.Dispose() = context.Dispose()

let renderFigures (renderer : IRenderer) figures =
    let pixels = renderFigureList figures
    pixels
        |> PSeq.map (PSeq.filter (fun ((x, y), _) -> x >= 0 && y >= 0 && x < renderer.Width && y < renderer.Height))
        |> Seq.iter (PSeq.iter (fun ((x, y), c) -> renderer.PutPixel x y c))

let renderGrid (renderer : IRenderer) spacing color =
    let xs = [spacing .. spacing .. renderer.Width - 1]
    let ys = [spacing .. spacing .. renderer.Height - 1]
    let colYs = [0 .. renderer.Height - 1]
    let rowXs = [0 .. renderer.Width - 1]
    xs |> List.map (fun x -> colYs |> PSeq.iter (fun y -> renderer.PutPixel x y color)) |> ignore
    ys |> List.map (fun y -> rowXs |> PSeq.iter (fun x -> renderer.PutPixel x y color)) |> ignore
   