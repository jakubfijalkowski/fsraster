[<RequireQualifiedAccess>]
module FsRaster.FigureColor

open System
open System.Windows.Media.Imaging

open FsRaster.Utils
open FsRaster.OctTree

type Texture = { Width : int; Height: int; Colors: Colors.ColorList }
type ReducedTexture = { Base: Texture; Colors: int; Reduced: Colors.ColorList; CachedTree : RGBTree }

[<ReferenceEquality>]
type Color = 
    | Color          of Colors.RawColor
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

let isTexture = function
    | Texture _        -> true
    | ReducedTexture _ -> true
    | _                -> false

let isReducedTexture = function
    | ReducedTexture _ -> true
    | _                -> false

let forceColor = getColor >> Color

let makeColor = Colors.fromUIColor >> Color

let reduceTexture toColors = function
    | Texture c ->
        let tree = prepareTree c.Colors
        let reduced = reduceTreeAndColors tree c.Colors toColors
        ReducedTexture { Base = c; Colors = toColors; Reduced = reduced; CachedTree = tree }
    | ReducedTexture c  -> ReducedTexture { c with Colors = toColors; Reduced = reduceTreeAndColors c.CachedTree c.Base.Colors toColors }
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
    for idx in 0 .. ctx.Width * ctx.Height - 1 do
        let pix = NativeInterop.NativePtr.get pixels idx
        output.[idx] <- Colors.rgbToARGB pix
    Texture { Width = ctx.Width; Height = ctx.Height; Colors = output }