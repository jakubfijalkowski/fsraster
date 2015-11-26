[<RequireQualifiedAccess>]
module FsRaster.FigureColor

open System
open System.Windows.Media.Imaging

open FsRaster.Utils
open FsRaster.OctTree

type RawColor = System.Windows.Media.Color
type ColorList = RawColor []

type ReducedTexture = { Width : int; Height: int; Original: ColorList; Colors: int; Reduced: ColorList; CachedTree : RGBTree * int }

[<ReferenceEquality>]
type Color = 
    | Color          of RawColor
    | Texture        of int * int * ColorList
    | ReducedTexture of ReducedTexture

let getColor = function
    | Color c           -> c
    | Texture (_, _, c) -> c.[0]
    | ReducedTexture c  -> c.Reduced.[0]

let getColorAt x y = function
    | Color c           -> c
    | Texture (w, h, c) -> c.[y * w + x]
    | ReducedTexture c  -> c.Reduced.[y * c.Width + x]

let getPatternSize = function
    | Color _           -> (1, 1)
    | Texture (w, h, _) -> (w, h)
    | ReducedTexture c  -> (c.Width, c.Height)

let fromColor = Color

let isTexture = function
    | Texture _        -> true
    | ReducedTexture _ -> true
    | _                -> false

let isReducedTexture = function
    | ReducedTexture _ -> true
    | _                -> false

let forceColor = getColor >> Color

let reduceTexture toColors = function
    | Texture (w, h, c) ->
        let tree = prepareReductionTree c
        let reduced = reducePalette c tree toColors
        ReducedTexture { Width = w; Height = h; Original = c; Colors = toColors; Reduced = reduced; CachedTree = tree }
    | ReducedTexture c  -> ReducedTexture { c with Colors = toColors; Reduced = reducePalette c.Original c.CachedTree toColors }
    | c -> c

let revertReduceTexture = function
    | ReducedTexture c -> Texture (c.Width, c.Height, c.Original)
    | c -> c

let asReducedTexture = function
    | ReducedTexture c -> c
    | _ -> failwith "Not reduced texture"

#nowarn "9"
let fromImage imgPath =
    let source = new BitmapImage(new Uri(imgPath, UriKind.Absolute))
    let wb = new WriteableBitmap(source)
    use ctx = wb.GetBitmapContext(ReadWriteMode.ReadOnly)
    let pixels = ctx.Pixels
    let mutable output = Array.zeroCreate (ctx.Width * ctx.Height)
    for y in [ 0 .. ctx.Height - 1] do
        for x in [ 0 .. ctx.Width - 1] do
            let idx = y * ctx.Width + x
            let pix = NativeInterop.NativePtr.get pixels idx
            let b = pix &&& 0xff
            let g = (pix >>> 8) &&& 0xff
            let r = (pix >>> 16) &&& 0xff
            output.[idx] <- RawColor.FromRgb(byte r, byte g, byte b)
    Texture (ctx.Width, ctx.Height, output)