module FsRaster.Figures

open System
open System.Windows.Media

open FSharp.Collections.ParallelSeq

open FsRaster.Utils

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

let private colorize c lst = PSeq.map (fun a -> (a, c)) lst
let distance ((x1, y1) : Point) ((x2, y2) : Point) =
    let x = x2 - x1
    let y = y2 - y1
    int <| Math.Sqrt (float (x * x + y * y))
let private withAlpha (c : Color) (i : float) =
    let clamp i = byte (max (min 255.0 i) 0.0)
    Color.FromArgb(byte (i * 255.0), c.R, c.G, c.B)

let private renderPoint (p, c) = PSeq.singleton (p, c)
let private renderLine' (x1', y1') (x2', y2') =
    if x1' = x2' && y1' = y2' then Seq.singleton (x1', y1')
    else
        let renderOctant x1 y1 x2 y2 =
            let dx = x2 - x1
            let dy = y2 - y1
            let d' = 2 * dy - dx
            let incE = 2 * dy
            let incNE = 2 * (dy - dx)
            let rec build acc d x y =
                if x >= x2 then acc
                else
                    let acc' = (x, y) :: acc
                    if d < 0
                    then build acc' (d + incE) (x + 1) y
                    else build acc' (d + incNE) (x + 1) (y + 1)
            build [] d' x1 y1 |> List.toSeq
        let dx = x2' - x1'
        let dy = y2' - y1'
        let result =
            match (dx, dy) with
            | (a, b) when dy < 0 && -dy <= dx -> renderOctant x1' -y1' x2' -y2' |> Seq.map (fun (x, y) -> (x, -y)) 
            | (a, b) when dy < 0 && -dy > dx  -> renderOctant -y1' x1' -y2' x2' |> Seq.map (fun (y, x) -> (x, -y))
            | (a, b) when dy > dx             -> renderOctant y1' x1' y2' x2'   |> Seq.map (fun (y, x) -> (x, y))
            | _                               -> renderOctant x1' y1' x2' y2'
        result
let private renderLine (p1, p2, c) = renderLine' p1 p2 |> colorize c

let replicateCirclePoints (x, y) = seq {
    yield (x, y)
    yield (-x, y)
    yield (x, -y)
    yield (-x, -y)
    yield (y, x)
    yield (-y, x)
    yield (y, -x)
    yield (-y, -x) }

let replicateCirclePointsWithColor (p, c) = replicateCirclePoints p |> Seq.map (fun x -> (x, c))

let private renderCircle' s r =
    let rec build acc d dE dSE x y =
        if y < x then acc
        else
            let acc' = (x, y) :: acc
            if d < 0
            then build acc' (d + dE) (dE + 2) (dSE + 2) (x + 1) y
            else build acc' (d + dSE) (dE + 2) (dSE + 4) (x + 1) (y - 1)
    build [] (1 - r) 3 (5 - 2 * r) 0 r
        |> PSeq.collect replicateCirclePoints
        |> PSeq.map ((+~) s)

let private renderCircle (s, r, c) = renderCircle' s r |> colorize c

let private renderAACircle (s, r, c) =
    let dist a b = ceil (sqrt (a * a - b * b)) - sqrt (a * a - b * b)
    let rec build acc x y t =
        if x <= y + 1 then acc
        else
            let y' = y + 1
            let d = dist (float r) (float y')
            let x' = if d < t then x - 1 else x
            let p1 = ((x', y'), withAlpha c (1.0 - d))
            let p2 = ((x' - 1, y'), withAlpha c d)
            build (p1 :: p2 :: acc) x' y' d
    build [((r, 0), c)] r 0 0.0
        |> PSeq.collect replicateCirclePointsWithColor
        |> PSeq.map (first ((+~) s))

let private renderRepeatedLine f (p1, p2, c, t) =
    if t < 3 then renderLine (p1, p2, c)
    else
        let addFix ((x1, y1), (x2, y2)) =
            match (x2 - x1, y2 - y1) with
            | 1, 1   -> f t (x2 - 1, y2)
            | 1, -1  -> f t (x2, y2 + 1)
            | -1, 1  -> f t (x2, y2 - 1)
            | -1, -1 -> f t (x2 + 1, y2)
            | _ -> []
        let line = renderLine' p1 p2 |> Seq.toList
        PSeq.append (line |> PSeq.collect (f t)) (line |> List.pairwise |> PSeq.collect addFix) |> colorize c

let private renderSquareLine =
    let renderSquare t (cx, cy) =
        let ht = t / 2
        let hor = [cx - ht .. cx + ht] |> List.collect (fun x -> [(x, cy - ht); (x, cy + ht)])
        let ver = [cy - ht .. cy + ht] |> List.collect (fun y -> [(cx - ht, y); (cx + ht, y)])
        List.append hor ver
    renderRepeatedLine renderSquare

let private renderDiamondLine =
    let renderDiamond t (cx, cy) =
        let ht = t / 2
        let up = [cy - ht .. cy] |> List.zip [0 .. ht] |> List.collect (fun (i, y) -> [(cx + i, y); (cx - i, y)])
        let down = [cy .. cy + ht] |> List.zip [ht .. -1 .. 0] |> List.collect (fun (i, y) -> [(cx + i, y); (cx - i, y)])
        List.append up down
    renderRepeatedLine renderDiamond

let private renderCircleLine =
    let intRenderCircle t s = renderCircle' s (int (ceil (float t / 2.0))) |> Seq.toList
    renderRepeatedLine intRenderCircle

let renderSingleFigure = function
    | Point  p    -> renderPoint p
    | Line   a    -> renderLine a
    | Circle c    -> renderCircle c
    | AntialiasedCircle c -> renderAACircle c
    | SquareLine s -> renderSquareLine s
    | DiamondLine s -> renderDiamondLine s
    | CircleLine s -> renderCircleLine s

let renderFigureList figs = PSeq.ordered figs |> PSeq.map renderSingleFigure |> PSeq.toList

type FigureBuilder = {
    Builder : FigureInfo -> Point list -> Figure;
    PointsLeft : int;
    Points : Point list;
    Preview : Point list -> FigureInfo -> Figure seq
}

let private buildLine' c = function
    | [(x1, _) as p1; (x2, _) as p2] ->
        let (p1', p2') = if x1 > x2 then (p2, p1) else (p1, p2)
        (p1', p2', c.Color)
    | _        -> failwith "Invalid number of points."
let setThickness i (p1, p2, c) = (p1, p2, c, i.Thickness)

let private buildPoint c = function
    | [p] -> Point (p, c.Color)
    | _   -> failwith "Invalid number of points."
let private buildLine c = buildLine' c >> Line
let private buildCircle c = function
    | [p1; p2] -> Circle (p1, distance p1 p2, c.Color)
    | _        -> failwith "Invalid number of points."
let private buildAACircle c = function
    | [p1; p2] -> AntialiasedCircle (p1, distance p1 p2, c.Color)
    | _        -> failwith "Invalid number of points."
let private buildSquareLine c = buildLine' c >> setThickness c >> SquareLine
let private buildDiamondLine c = buildLine' c >> setThickness c >> DiamondLine
let private buildCircleLine c = buildLine' c >> setThickness c >> CircleLine

let private previewBuilder b pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (b c pts)
    | _        -> Seq.empty

let private previewCircle pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (Circle (p1, distance p1 p2, c.Color))
    | _        -> Seq.empty
let private previewAACircle pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (AntialiasedCircle (p1, distance p1 p2, c.Color))
    | _        -> Seq.empty

let getFigureBuilder = function
    | Point _  -> { Builder = buildPoint; PointsLeft = 1; Points = []; Preview = fun _ _ -> Seq.empty }
    | Line _   -> { Builder = buildLine; PointsLeft = 2; Points = []; Preview = previewBuilder buildLine }
    | Circle _ -> { Builder = buildCircle; PointsLeft = 2; Points = []; Preview = previewCircle }
    | AntialiasedCircle _ -> { Builder = buildAACircle; PointsLeft = 2; Points = []; Preview = previewAACircle }
    | SquareLine _ -> { Builder = buildSquareLine; PointsLeft = 2; Points = []; Preview = previewBuilder buildSquareLine }
    | DiamondLine _ -> { Builder = buildDiamondLine; PointsLeft = 2; Points = []; Preview = previewBuilder buildDiamondLine }
    | CircleLine _ -> { Builder = buildCircleLine; PointsLeft = 2; Points = []; Preview = previewBuilder buildCircleLine }

let processBuildingFigure builder pt c =
    let points = builder.Points @ [pt]
    if builder.PointsLeft = 1
    then Choice1Of2 (builder.Builder c points)
    else Choice2Of2 { builder with Points = points; PointsLeft = builder.PointsLeft - 1 }

let previewFigure ({ Preview = prev; Points = pts } as builder) pt c = prev (pts @ [pt]) c

let private isPointHit (p1, _) p2 = 
    let d = distance p1 p2
    if d < 10 then Some d else None
let private isLineHit (x, y) (((x1, y1) as p1), ((x2, y2) as p2), _) =
    let d' = max (distance p1 p2) 1
    let d = (abs ((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1)) / d'
    let isInside = x >= x1 && x <= x2 && y >= (min y1 y2) && y <= (max y1 y2)
    if d < 20 && isInside then Some d else None
let private isCircleHit pt (s, r, _) =
    let d = distance s pt
    if d <= r then Some (r - d) else None

let isFigureHit pt = function
    | Point p  -> isPointHit p pt
    | Line l   -> isLineHit pt l
    | Circle c -> isCircleHit pt c
    | AntialiasedCircle c -> isCircleHit pt c
    | SquareLine (p1, p2, c, _) -> isLineHit pt (p1, p2, c)
    | DiamondLine (p1, p2, c, _) -> isLineHit pt (p1, p2, c)
    | CircleLine (p1, p2, c, _) -> isLineHit pt (p1, p2, c)

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