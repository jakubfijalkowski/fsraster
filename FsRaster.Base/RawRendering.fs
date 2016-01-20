﻿module FsRaster.RawRendering

open System
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Diagnostics.CodeAnalysis

// Required to fight thread access exceptions
type CachedBitmapContext =
    {
        Context : BitmapContext;
        Width : int;
        Height : int
    }

    interface IDisposable with
        member x.Dispose() = x.Context.Dispose()

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

let putPixelAlpha ctx x y c =
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

let getPixel ctx x y =
    let index = y * ctx.Width + x
    let pixels = ctx.Context.Pixels
    NativeInterop.NativePtr.get pixels index

let acquireRenderer (bmp : WriteableBitmap) =
    let ctx = bmp.GetBitmapContext(ReadWriteMode.ReadWrite)
    { Context = ctx; Width = ctx.Width; Height = ctx.Height }