module FsRaster.Figures

open System
open System.Windows.Media

open FSharp.Collections.ParallelSeq

open FsRaster.Utils

type Point = int * int
type RenderPrimitive =
    | PrimPixel of Point * Color
    | PrimLine  of (int * int * int * Color)
type Rectangle = int * int * int * int

// "swim" operators - vector addition/subtraction
let inline (+~) ((x1, y1) : Point) ((x2, y2) : Point) = (x1 + x2, y1 + y2)
let inline (-~) ((x1, y1) : Point) ((x2, y2) : Point) = (x1 - x2, y1 - y2)

type Figure =
    | Point             of (Point * FigureColor.Color)
    | Line              of (Point * Point * FigureColor.Color)
    | Circle            of (Point * int * FigureColor.Color)
    | AntialiasedCircle of (Point * int * FigureColor.Color)
    | SquareLine        of (Point * Point * FigureColor.Color * int)
    | DiamondLine       of (Point * Point * FigureColor.Color * int)
    | CircleLine        of (Point * Point * FigureColor.Color * int)
    | Polyline          of (Point list * FigureColor.Color)
    | Polygon           of (Point list * FigureColor.Color)
    | FilledPolygon     of (Point list * FigureColor.Color)
    | Brush             of (Point list * FigureColor.Color)
    | FilledBrush       of (Point list * FigureColor.Color)

type FigureInfo = { Color : FigureColor.Color; Thickness: int; Filled: bool option }

let getFigureInfo = function
    | Point (_, c)                -> { Color = c; Thickness = 1; Filled = None       }
    | Line (_, _, c)              -> { Color = c; Thickness = 1; Filled = None       }
    | Circle (_, _, c)            -> { Color = c; Thickness = 1; Filled = None       }
    | AntialiasedCircle (_, _, c) -> { Color = c; Thickness = 1; Filled = None       }
    | SquareLine (_, _, c, t)     -> { Color = c; Thickness = t; Filled = None       }
    | DiamondLine (_, _, c, t)    -> { Color = c; Thickness = t; Filled = None       }
    | CircleLine (_, _, c, t)     -> { Color = c; Thickness = t; Filled = None       }
    | Polyline (_, c)             -> { Color = c; Thickness = 1; Filled = None       }
    | Polygon (_, c)              -> { Color = c; Thickness = 1; Filled = Some false }
    | FilledPolygon (_, c)        -> { Color = c; Thickness = 1; Filled = Some true  }
    | Brush (_, c)                -> { Color = c; Thickness = 1; Filled = Some false }
    | FilledBrush (_, c)          -> { Color = c; Thickness = 1; Filled = Some true  }

let availableFigures = [
    Point ((0,0), FigureColor.fromColor Colors.Red)
    Line ((0,0), (0,0), FigureColor.fromColor Colors.Red);
    Circle ((0,0), 0, FigureColor.fromColor Colors.Red);
    AntialiasedCircle ((0,0), 0, FigureColor.fromColor Colors.Red);
    SquareLine ((0,0), (0,0), FigureColor.fromColor Colors.Red, 1);
    DiamondLine ((0,0), (0,0), FigureColor.fromColor Colors.Red, 1);
    CircleLine ((0,0), (0,0), FigureColor.fromColor Colors.Red, 1);
    Polyline ([], FigureColor.fromColor Colors.Red);
    Polygon ([], FigureColor.fromColor Colors.Red);
    FilledPolygon ([], FigureColor.fromColor Colors.Red);
    Brush ([], FigureColor.fromColor Colors.Red);
    FilledBrush ([], FigureColor.fromColor Colors.Red)
]

let distance ((x1, y1) : Point) ((x2, y2) : Point) =
    let x = x2 - x1
    let y = y2 - y1
    int <| Math.Sqrt (float (x * x + y * y))

let updateFigure c fig =
    let filled = Option.opt false c.Filled
    let polygon = if filled then FilledPolygon else Polygon
    let brush = if filled then FilledBrush else Brush
    match fig with
    | Point (p, _)                -> Point (p, c.Color)
    | Line (p1, p2, _)            -> Line (p1, p2, c.Color)
    | Circle (s, r, _)            -> Circle (s, r, c.Color)
    | AntialiasedCircle (s, r, _) -> AntialiasedCircle (s, r, c.Color)
    | SquareLine (p1, p2, _, _)   -> SquareLine (p1, p2, c.Color, c.Thickness)
    | DiamondLine (p1, p2, _, _)  -> DiamondLine (p1, p2, c.Color, c.Thickness)
    | CircleLine (p1, p2, _, _)   -> CircleLine (p1, p2, c.Color, c.Thickness)
    | Polyline (p, _)             -> Polyline (p, c.Color)
    | Polygon (p, _)              -> polygon (p, c.Color)
    | FilledPolygon (p, _)        -> polygon (p, c.Color)
    | Brush (p, _)                -> brush (p, c.Color)
    | FilledBrush (p, _)          -> brush (p, c.Color)

let moveFigure pt = function
    | Point (p, c)                -> Point (p +~ pt, c)
    | Line (p1, p2, c)            -> Line (p1 +~ pt, p2 +~ pt, c)
    | Circle (s, r, c)            -> Circle (s +~ pt, r, c)
    | AntialiasedCircle (s, r, c) -> AntialiasedCircle (s +~ pt, r, c)
    | SquareLine (p1, p2, c, t)   -> SquareLine (p1 +~ pt, p2 +~ pt, c, t)
    | DiamondLine (p1, p2, c, t)  -> DiamondLine (p1 +~ pt, p2 +~ pt, c, t)
    | CircleLine (p1, p2, c, t)   -> CircleLine (p1 +~ pt, p2 +~ pt, c, t)
    | Polyline (p, c)             -> Polyline (List.map ((+~) pt) p, c)
    | Polygon (p, c)              -> Polygon (List.map ((+~) pt) p, c)
    | FilledPolygon (p, c)        -> FilledPolygon (List.map ((+~) pt) p, c)
    | Brush (p, c)                -> Brush (List.map ((+~) pt) p, c)
    | FilledBrush (p, c)          -> FilledBrush (List.map ((+~) pt) p, c)

let crossProd (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

let isOnRect (x, y) (x1, y1) (x2, y2) =
    min x1 x2 <= x && x <= max x1 x2 &&
    min y1 y2 <= y && y <= max y1 y2

let isOnLine p (p1, p2) = distance p1 p + distance p p2 = distance p1 p2

let doLinesCross (p1, p2) (p3, p4) =
    let d1 = crossProd (p4 -~ p3) (p1 -~ p3)
    let d2 = crossProd (p4 -~ p3) (p2 -~ p3)
    let d3 = crossProd (p2 -~ p1) (p3 -~ p1)
    let d4 = crossProd (p2 -~ p1) (p4 -~ p1)
    let d12 = sign d1 * sign d2
    let d34 = sign d3 * sign d4
    match (d12, d34) with
    | (a, b) when a > 0 || b > 0 -> false
    | (a, b) when a < 0 || b < 0 -> true
    | _ -> isOnRect p1 p3 p4 || isOnRect p2 p3 p4 || isOnRect p3 p1 p2 || isOnRect p4 p1 p2

let distanceFromLine (x, y) (((x1, y1) as p1), ((x2, y2) as p2)) =
    let d' = max (distance p1 p2) 1
    (abs ((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1)) / d'

let makeConnected = function
    | (p1 :: p2 :: _) as pts -> pts @ [p1]
    | _                      -> []

let moveRect (x, y) (left, top, right, bottom) = (left + x, top + y, right + x, bottom + y)
let resizeRectMin m (x, y) (left, top, right, bottom) =
    let width = max m (right + x - left)
    let height = max m (bottom + y - top)
    (left, top, left + width, top + height)

let generateGrid width height spacing color =
    let xs = [ spacing .. spacing .. width - 1 ]
    let ys = [ spacing .. spacing .. height - 1 ]
    [ for x in xs -> Line ((x, 0), (x, height), FigureColor.fromColor color) ] @
    [ for y in ys -> Line ((0, y), (width, y), FigureColor.fromColor color) ]
    
let rec inPairs = function
    | a :: b :: rest -> (a, b) :: inPairs rest
    | _              -> []