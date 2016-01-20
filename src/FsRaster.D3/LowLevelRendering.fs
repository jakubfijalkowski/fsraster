module FsRaster.D3.LowLevelRendering

open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Renderer

#nowarn "9"

let putPrimPoints ctx (points : Point3D array) =
    let pixels = ctx.Context.Pixels
    let w = ctx.Width
    for (x, y, _) in points do
        let idx = y * w + x
        NativeInterop.NativePtr.set pixels idx 0xffffffff

let putPrimitives ctx prims =
    for p in prims do
        match p with
        | PrimPoints pts -> putPrimPoints ctx pts

let drawModel renderer context model =
    let w = double context.Width
    let h = double context.Height
    renderModel renderer w h model |> putPrimitives context