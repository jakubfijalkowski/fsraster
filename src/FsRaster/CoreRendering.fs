module FsRaster.CoreRendering

open System
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Diagnostics.CodeAnalysis

open Microsoft.FSharp.NativeInterop

open FSharp.Collections.ParallelSeq

open FsRaster.RawRendering
open FsRaster.Figures
open FsRaster.FigureRendering

#nowarn "9"

let putSolidLine ctx x1' y x2' c = 
    let initialBlockSize = 32
    let x1 = max x1' 0
    let x2 = min x2' (ctx.Width - 1)
    let w = x2 - x1
    if x2 >= 0 && x1 < ctx.Width && y >= 0 && y < ctx.Height && w >= 0 then 
        // Here, we operate on ints (as pixels is nativeptr<int>)
        let startIdx = y * ctx.Width + x1
        let pixels = ctx.Pixels
        for i in [ 0..min w (initialBlockSize - 1) ] do
            NativeInterop.NativePtr.set pixels (startIdx + i) c
        // But here, we must switch to bytes, as BlockCopy operates on bytes, not on ints
        let startIdx' = startIdx * 4
        let endIndex = (y * ctx.Width + x2) * 4
        let mutable index = (startIdx + initialBlockSize) * 4
        let mutable block = initialBlockSize * 4
        while index <= endIndex do
            copyMemory ctx.Pixels startIdx' ctx.Pixels index (min block (endIndex - index + 4))
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
        let pixels = ctx.Pixels
        let originX, originY = prim.Origin
        let texY = abs ((originY - y) % prim.Texture.Height)
        for x in x1..x2 do
            let texX = abs (x - originX) % prim.Texture.Width
            let c = FigureColor.getTexPixel prim.Texture texX texY
            NativePtr.set pixels (startIdx + x) c

let putPrimitive ctx = 
    function 
    | PrimPixel((x, y), c) -> putPixelAlpha ctx x y c
    | PrimLine(x1, y, x2, c) -> putSolidLine ctx x1 y x2 c
    | PrimTexLine t -> putTexLine ctx t

type YUVPlane = 
    | PlaneY
    | PlaneU
    | PlaneV

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
    let pixels = ctx.Pixels
    let w = ctx.Width
    let h = ctx.Height
    let len = w * h - 1
    for i in 0..len do
        let pix = NativePtr.get pixels i
        let yuv = getYUVFromPixel pix |> getPlane plane
        let rgb = yuvToRGB yuv
        NativePtr.set pixels i rgb

let streamPixels ctx rect = 
    let maxW = ctx.Width
    let maxH = ctx.Height
    let x1, y1, x2, y2 = clipRect rect maxW maxH
    let pixels = ctx.Pixels
    [| for y in [ y1..y2 - 1 ] do
           let scanline = y * ctx.Width
           for i in [ scanline + x1..scanline + x2 - 1 ] do
               yield NativePtr.get pixels i |]

let mapPixelsXY ctx rect f = 
    let maxW = ctx.Width - 1
    let maxH = ctx.Height - 1
    let x1, y1, x2, y2 = clipRect rect maxW maxH
    let pixels = ctx.Pixels
    for y in [ y1..y2 ] do
        for x in [ x1..x2 ] do
            let i = y * ctx.Width + x
            let pix = NativeInterop.NativePtr.get pixels i
            let newPix = f x y pix
            NativeInterop.NativePtr.set pixels i newPix

let foldPixelsXY ctx rect f state = 
    let maxW = ctx.Width - 1
    let maxH = ctx.Height - 1
    let x1, y1, x2, y2 = clipRect rect maxW maxH
    let pixels = ctx.Pixels
    let mutable state' = state
    for y in [ y1..y2 ] do
        for x in [ x1..x2 ] do
            let i = y * ctx.Width + x
            let pix = NativeInterop.NativePtr.get pixels i
            state' <- f state' x y pix
    state'

type IRenderer = 
    inherit IDisposable
    abstract Width : int
    abstract Height : int
    abstract Clear : Color -> unit
    abstract PutColor : int -> int -> Color -> unit
    abstract GetPixel : int -> int -> int
    abstract PutPixel : int -> int -> int -> unit
    abstract PutPrimitive : RenderPrimitive -> unit
    abstract ExtractYUVPlane : YUVPlane -> unit
    abstract StreamPixels : Rectangle -> int array
    abstract Map : Rectangle -> (int -> int) -> unit
    abstract MapXY : Rectangle -> (int -> int -> int -> int) -> unit
    abstract Fold : Rectangle -> ('a -> int -> 'a) -> 'a -> 'a
    abstract FoldXY : Rectangle -> ('a -> int -> int -> int -> 'a) -> 'a -> 'a
    abstract Context : PixelBufferContext

type BitmapRenderer(context : BitmapContext) = 
    
    let cachedContext = 
        { Pixels = context.Pixels
          Width = context.Width
          Height = context.Height }
    
    interface IRenderer with
        member this.Width = cachedContext.Width
        member this.Height = cachedContext.Height
        member this.Clear color = clearBitmap cachedContext (Colors.fromUIColor color)
        member this.PutColor x y color = putPixelAlpha cachedContext x y (Colors.fromUIColor color)
        member this.GetPixel x y = getPixel cachedContext x y
        member this.PutPixel x y c = putPixel cachedContext x y c
        member this.PutPrimitive prim = putPrimitive cachedContext prim
        member this.ExtractYUVPlane plane = extractYUVPlane cachedContext plane
        member this.StreamPixels rect = streamPixels cachedContext rect
        member this.Map rect f = mapPixelsXY cachedContext rect (fun _ _ p -> f p)
        member this.MapXY rect f = mapPixelsXY cachedContext rect f
        member this.Fold rect f state = foldPixelsXY cachedContext rect (fun s _ _ p -> f s p) state
        member this.FoldXY rect f state = foldPixelsXY cachedContext rect f state
        member this.Context = cachedContext
    
    interface IDisposable with
        member x.Dispose() = context.Dispose()

let renderFigures (renderer : IRenderer) figures = 
    let pixels = renderFigureList figures
    pixels |> List.iter (PSeq.iter renderer.PutPrimitive)
