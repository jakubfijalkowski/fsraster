[<RequireQualifiedAccess>]
module FsRaster.Colors

type UIColor = System.Windows.Media.Color
type RawColor = int
type ColorList = RawColor []

let inline getA c = c >>> 24 &&& 0xff
let inline getR c = c >>> 16 &&& 0xff
let inline getG c = c >>> 8 &&& 0xff
let inline getB c = c &&& 0xff
let inline setA c a = a <<< 24 ||| c
let inline fromRGB r g b = 0xff000000 ||| (r <<< 16) ||| (g <<< 8) ||| b
let inline fromARGB a r g b = (a <<< 24) ||| (r <<< 16) ||| (g <<< 8) ||| b
let inline argbToRGB c = c &&& 0x00ffffff
let inline rgbToARGB c = c ||| 0xff000000
let inline fromUIColor (c : UIColor) =
    let a = int c.A
    let r = int c.R
    let g = int c.G
    let b = int c.B
    fromARGB a r g b

let inline toUIColor c =
    let a = byte (getA c)
    let r = byte (getR c)
    let g = byte (getG c)
    let b = byte (getB c)
    UIColor.FromArgb(a, r, g, b)
