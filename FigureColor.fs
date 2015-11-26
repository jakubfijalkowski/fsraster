[<RequireQualifiedAccess>]
module FsRaster.FigureColor

open System
open System.Windows.Media.Imaging

open FsRaster.Utils
open FsRaster.OctTree

type RawColor = System.Windows.Media.Color
type ColorList = RawColor []

type Color = 
    | Color          of RawColor
    | Texture        of int * int * ColorList
    | ReducedTexture of int * int * ColorList * ColorList

let getColor = function
    | Color c                     -> c
    | Texture (_, _, c)           -> c.[0]
    | ReducedTexture (_, _, _, c) -> c.[0]

let getColorAt x y = function
    | Color c                     -> c
    | Texture (w, h, c)           -> c.[y * w + x]
    | ReducedTexture (w, h, _, c) -> c.[y * w + x]

let getPatternSize = function
    | Color _                     -> (1, 1)
    | Texture (w, h, _)           -> (w, h)
    | ReducedTexture (w, h, _, _) -> (w, h)

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
    | Texture (w, h, c) -> ReducedTexture (w, h, c, reducePalette c toColors)
    | ReducedTexture (w, h, c, _) -> ReducedTexture (w, h, c, reducePalette c toColors)
    | c -> c

let revertReduceTexture = function
    | ReducedTexture (w, h, c, _) -> Texture (w, h, c)
    | c -> c

#nowarn "9"
let fromImage imgPath =
    let source = new BitmapImage(new Uri(imgPath, UriKind.Absolute))
    let wb = new WriteableBitmap(source)
    use ctx = wb.GetBitmapContext(ReadWriteMode.ReadOnly)
    let pixels = ctx.Pixels
    let mutable output = []
    for y in [ 0 .. ctx.Height - 1] do
        for x in [ 0 .. ctx.Width - 1] do
            let idx = y * ctx.Width + x
            let pix = NativeInterop.NativePtr.get pixels idx
            let b = pix &&& 0xff
            let g = (pix >>> 8) &&& 0xff
            let r = (pix >>> 16) &&& 0xff
            output <- System.Windows.Media.Color.FromRgb(byte r, byte g, byte b) :: output
    Texture (ctx.Width, ctx.Height, output |> List.rev |> List.toArray)