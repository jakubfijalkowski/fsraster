module FsRaster.D3.CoreRenderer3D

// This is mostly FsRaster.CoreRenderer, but adjusted for a different purpose
// And finally it isn't an interface

open System
open System.Windows.Media.Imaging

open FsRaster.RawRendering

type CoreRenderer3D =
    { Context : CachedBitmapContext }

    interface IDisposable with
        member x.Dispose() = x.Context.Context.Dispose()


let acquireRenderer (bmp : WriteableBitmap) =
    let ctx = bmp.GetBitmapContext(ReadWriteMode.ReadWrite)
    let bmpCtx = { Context = ctx; Width = ctx.Width; Height = ctx.Height }
    { Context = bmpCtx }

let clearColor r c = clearBitmap r.Context c

let putPixel r = FsRaster.RawRendering.putPixel r.Context