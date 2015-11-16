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

let convertColor c = WriteableBitmapExtensions.ConvertColor c

// https://github.com/teichgraf/WriteableBitmapEx/blob/master/Source/WriteableBitmapEx/WriteableBitmapBaseExtensions.cs#L79-L105
let clearBitmap ctx c =
    let color = convertColor c
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
let putColor ctx x y c =
    let color = convertColor c
    let pixels = ctx.Context.Pixels
    let index = y * ctx.Width + x
    NativeInterop.NativePtr.set pixels index color

// http://stackoverflow.com/questions/12011081/alpha-blending-2-rgba-colors-in-c
[<SuppressMessage("NumberOfItems", "MaxNumberOfFunctionParameters")>]
let blendPixels r1 g1 b1 r2 g2 b2 a' =
    let a = a' + 1
    let inv_a = 256 - a'
    let r' = (a * r2 + inv_a * r1) >>> 8
    let g' = (a * g2 + inv_a * g1) >>> 8
    let b' = (a * b2 + inv_a * b1) >>> 8
    (0xff000000) ||| (r' <<< 16) ||| (g' <<< 8) ||| b'

let putColorAlpha ctx x y (c : Color) =
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

let putPixel ctx x y c =
    let index = y * ctx.Width + x
    let pixels = ctx.Context.Pixels
    NativeInterop.NativePtr.set pixels index c

let getPixel ctx x y =
    let index = y * ctx.Width + x
    let pixels = ctx.Context.Pixels
    NativeInterop.NativePtr.get pixels index

type IRenderer =
    inherit IDisposable

    abstract member Width : int
    abstract member Height : int

    abstract member Clear : Color -> unit
    abstract member PutColor : int -> int -> Color -> unit

    abstract member GetPixel : int -> int -> int
    abstract member PutPixel : int -> int -> int -> unit
       
type BitmapRenderer(context : BitmapContext) =
    let cachedContext = { Context = context; Width = context.Width; Height = context.Height }

    interface IRenderer with
        member this.Width = cachedContext.Width
        member this.Height = cachedContext.Height

        member this.Clear color = clearBitmap cachedContext color
        member this.PutColor x y color = putColorAlpha cachedContext x y color

        member this.GetPixel x y = getPixel cachedContext x y
        member this.PutPixel x y c = putPixel cachedContext x y c

    interface IDisposable with
        member x.Dispose() = context.Dispose()

let renderFigures (renderer : IRenderer) figures =
    let pixels = renderFigureList figures
    pixels
        |> List.map (PSeq.filter (fun ((x, y), _) -> x >= 0 && y >= 0 && x < renderer.Width && y < renderer.Height))
        |> List.iter (PSeq.iter (fun ((x, y), c) -> renderer.PutColor x y c))