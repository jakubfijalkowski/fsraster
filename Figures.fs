module FsRaster.Figures

open System
open System.Windows.Media

open FSharp.Collections.ParallelSeq

type Point = int * int
type Pixel = Point * Color

// "swim" operators - vector addition/subtraction
let inline (+~) ((x1, y1) : Point) ((x2, y2) : Point) = (x1 + x2, y1 + y2)
let inline (-~) ((x1, y1) : Point) ((x2, y2) : Point) = (x1 - x2, y1 - y2)

type Figure =
    | Point of (Point * Color)
    | Line of (Point * Point * Color)
    | Circle of (Point * int * Color)
    | AntialiasedCircle of (Point * int * Color)
    | SquareLine of (Point * Point * Color * int)
    | DiamondLine of (Point * Point * Color * int)
    | CircleLine of (Point * Point * Color * int)

type FigureInfo = { Color : Color; Thickness: int }

let getFigureInfo = function
    | Point (_, c)     -> { Color = c; Thickness = 1 }
    | Line (_, _, c)   -> { Color = c; Thickness = 1 }
    | Circle (_, _, c) -> { Color = c; Thickness = 1 }
    | AntialiasedCircle (_, _, c) -> { Color = c; Thickness = 1 }
    | SquareLine (_, _, c, t) -> { Color = c; Thickness = t }
    | DiamondLine (_, _, c, t) -> { Color = c; Thickness = t }
    | CircleLine (_, _, c, t) -> { Color = c; Thickness = t }

let availableFigures = [
    Point ((0,0), Colors.Red)
    Line ((0,0), (0,0), Colors.Red);
    Circle ((0,0), 0, Colors.Red);
    AntialiasedCircle ((0,0), 0, Colors.Red);
    SquareLine ((0,0), (0,0), Colors.Red, 1);
    DiamondLine ((0,0), (0,0), Colors.Red, 1);
    CircleLine ((0,0), (0,0), Colors.Red, 1)
]

let shortDescriptionOf fig = fig.GetType().Name.ToLower()
let longDescriptionOf fig = sprintf "%A" fig

let distance ((x1, y1) : Point) ((x2, y2) : Point) =
    let x = x2 - x1
    let y = y2 - y1
    int <| Math.Sqrt (float (x * x + y * y))

let updateFigure c = function
    | Point (p, _)     -> Point (p, c.Color)
    | Line (p1, p2, _) -> Line (p1, p2, c.Color)
    | Circle (s, r, _) -> Circle (s, r, c.Color)
    | AntialiasedCircle (s, r, _) -> AntialiasedCircle (s, r, c.Color)
    | SquareLine (p1, p2, _, _) -> SquareLine (p1, p2, c.Color, c.Thickness)
    | DiamondLine (p1, p2, _, _) -> DiamondLine (p1, p2, c.Color, c.Thickness)
    | CircleLine (p1, p2, _, _) -> CircleLine (p1, p2, c.Color, c.Thickness)

let moveFigure pt = function
    | Point (p, c)     -> Point (p +~ pt, c)
    | Line (p1, p2, c) -> Line (p1 +~ pt, p2 +~ pt, c)
    | Circle (s, r, c) -> Circle (s +~ pt, r, c)
    | AntialiasedCircle (s, r, c) -> AntialiasedCircle (s +~ pt, r, c)
    | SquareLine (p1, p2, c, t) -> SquareLine (p1 +~ pt, p2 +~ pt, c, t)
    | DiamondLine (p1, p2, c, t) -> DiamondLine (p1 +~ pt, p2 +~ pt, c, t)
    | CircleLine (p1, p2, c, t) -> CircleLine (p1 +~ pt, p2 +~ pt, c, t)