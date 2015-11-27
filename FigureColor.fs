[<RequireQualifiedAccess>]
module FsRaster.FigureColor

open System
open System.Windows.Media.Imaging

open FsRaster.Utils
open FsRaster.OctTree

type RawColor = System.Windows.Media.Color
type ColorList = RawColor []

type Texture = { Width : int; Height: int; Colors: ColorList }
type ReducedTexture = { Base: Texture; Colors: int; Reduced: ColorList; CachedTree : RGBTree * int }

[<ReferenceEquality>]
type Color = 
    | Color          of RawColor
    | Texture        of Texture
    | ReducedTexture of ReducedTexture

let getColor = function
    | Color c          -> c
    | Texture c        -> c.Colors.[0]
    | ReducedTexture c -> c.Reduced.[0]

let getColorAt x y = function
    | Color c          -> c
    | Texture c        -> c.Colors.[y * c.Width + x]
    | ReducedTexture c -> c.Reduced.[y * c.Base.Width + x]

let getPatternSize = function
    | Color _          -> (1, 1)
    | Texture c        -> (c.Width, c.Height)
    | ReducedTexture c -> (c.Base.Width, c.Base.Height)

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
    | Texture c ->
        let tree = prepareReductionTree c.Colors
        let reduced = reducePalette c.Colors tree toColors
        ReducedTexture { Base = c; Colors = toColors; Reduced = reduced; CachedTree = tree }
    | ReducedTexture c  -> ReducedTexture { c with Colors = toColors; Reduced = reducePalette c.Base.Colors c.CachedTree toColors }
    | c -> c

let revertReduceTexture = function
    | ReducedTexture c -> Texture c.Base
    | c -> c

let asReducedTexture = function
    | ReducedTexture c -> c
    | _ -> failwith "Not a reduced texture"

let getTextureInfo = function
    | Texture c        -> c
    | ReducedTexture c -> { Width = c.Base.Width; Height = c.Base.Height; Colors = c.Reduced }
    | _ -> failwith "Not a texture"

let getTexPixel (tex : Texture) x y = tex.Colors.[y * tex.Width + x]

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
    Texture { Width = ctx.Width; Height = ctx.Height; Colors = output }