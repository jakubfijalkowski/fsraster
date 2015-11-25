[<RequireQualifiedAccess>]
module FsRaster.FigureColor

open System
open System.Windows.Media.Imaging

type Color = 
    | Color         of System.Windows.Media.Color
    | BitmapPattern of int * int * System.Windows.Media.Color []

let getColor = function
    | Color c                 -> c
    | BitmapPattern (_, _, c) -> c.[0]

let getColorAt x y = function
    | Color c -> c
    | BitmapPattern (w, h, c) -> c.[y * w + x]

let getPatternSize = function
    | Color _ -> (1, 1)
    | BitmapPattern (w, h, _) -> (w, h)

let fromColor = Color

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
    BitmapPattern (ctx.Width, ctx.Height, output |> List.rev |> List.toArray)
