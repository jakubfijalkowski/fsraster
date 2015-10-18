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

let getFigureColor = function
    | Point (_, c)     -> c
    | Line (_, _, c)   -> c
    | Circle (_, _, c) -> c
    | AntialiasedCircle (_, _, c) -> c

let availableFigures = [
    Point ((0,0), Colors.Red)
    Line ((0,0), (0,0), Colors.Red);
    Circle ((0,0), 0, Colors.Red);
    AntialiasedCircle ((0,0), 0, Colors.Red)
]

let shortDescriptionOf fig = fig.GetType().Name.ToLower()
let longDescriptionOf fig = sprintf "%A" fig

let private colorize c lst = PSeq.map (fun a -> (a, c)) lst
let distance ((x1, y1) : Point) ((x2, y2) : Point) =
    let x = x2 - x1
    let y = y2 - y1
    int <| Math.Sqrt (float (x * x + y * y))
let private intensify (c : Color) (i : float) =
    let clamp i = byte (max (min 255.0 i) 0.0)
    Color.FromArgb(byte (i * 255.0), c.R, c.G, c.B)

let private renderPoint (p, c) = PSeq.singleton (p, c)
let private renderLine ((x1', y1'), (x2', y2'), c) =
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
    result |> colorize c


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

let private renderCircle (s, r, c) =
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
        |> colorize c

let private renderAACircle (s, r, c) =
    let dist a b = ceil (sqrt (a * a - b * b)) - sqrt (a * a - b * b)
    let rec build acc x y t =
        if x <= y + 1 then acc
        else
            let y' = y + 1
            let d = dist (float r) (float y')
            let x' = if d < t then x - 1 else x
            let p1 = ((x', y'), intensify c (1.0 - d))
            let p2 = ((x' - 1, y'), intensify c d)
            build (p1 :: p2 :: acc) x' y' d
    build [((r, 0), c)] r 0 0.0
        |> PSeq.collect replicateCirclePointsWithColor
        |> PSeq.map (first ((+~) s))

let renderSingleFigure = function
    | Point  p    -> renderPoint p
    | Line   a    -> renderLine a
    | Circle c    -> renderCircle c
    | AntialiasedCircle c -> renderAACircle c

let renderFigureList figs = PSeq.ordered figs |> PSeq.map renderSingleFigure |> PSeq.toList

type FigureBuilder = {
    Builder : Color -> Point list -> Figure;
    PointsLeft : int;
    Points : Point list;
    Preview : Point list -> Color -> Figure seq
}

let private buildPoint c = function
    | [p] -> Point (p, c)
    | _   -> failwith "Invalid number of points."
let private buildLine c = function
    | [(x1, _) as p1; (x2, _) as p2] ->
        let (p1', p2') = if x1 > x2 then (p2, p1) else (p1, p2)
        Line (p1', p2', c)
    | _        -> failwith "Invalid number of points."
let private buildCircle c = function
    | [p1; p2] -> Circle (p1, distance p1 p2, c)
    | _        -> failwith "Invalid number of points."
let private buildAACircle c = function
    | [p1; p2] -> AntialiasedCircle (p1, distance p1 p2, c)
    | _        -> failwith "Invalid number of points."

let private previewLine pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (buildLine c pts)
    | _        -> Seq.empty
let private previewCircle pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (Circle (p1, distance p1 p2, c))
    | _        -> Seq.empty
let private previewAACircle pts c =
    match pts with
    | [p1; p2] -> Seq.singleton (AntialiasedCircle (p1, distance p1 p2, c))
    | _        -> Seq.empty

let getFigureBuilder = function
    | Point _  -> { Builder = buildPoint; PointsLeft = 1; Points = []; Preview = fun _ _ -> Seq.empty }
    | Line _   -> { Builder = buildLine; PointsLeft = 2; Points = []; Preview = previewLine }
    | Circle _ -> { Builder = buildCircle; PointsLeft = 2; Points = []; Preview = previewCircle }
    | AntialiasedCircle _ -> { Builder = buildAACircle; PointsLeft = 2; Points = []; Preview = previewAACircle }

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
    let d = (abs ((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1)) / distance p1 p2
    let isOutside = x < x1 || x > x2 || y < (min y1 y2) || y > (max y1 y2)
    if d < 10 && not isOutside then Some d else None
let private isCircleHit pt (s, r, _) =
    let d = distance s pt
    if d <= r then Some (r - d) else None

let isFigureHit pt = function
    | Point p  -> isPointHit p pt
    | Line l   -> isLineHit pt l
    | Circle c -> isCircleHit pt c
    | AntialiasedCircle c -> isCircleHit pt c

let updateFigureColor c = function
    | Point (p, _)     -> Point (p, c)
    | Line (p1, p2, _) -> Line (p1, p2, c)
    | Circle (s, r, _) -> Circle (s, r, c)
    | AntialiasedCircle (s, r, _) -> AntialiasedCircle (s, r, c)

let moveFigure pt = function
    | Point (p, c)     -> Point (p +~ pt, c)
    | Line (p1, p2, c) -> Line (p1 +~ pt, p2 +~ pt, c)
    | Circle (s, r, c) -> Circle (s +~ pt, r, c)
    | AntialiasedCircle (s, r, c) -> AntialiasedCircle (s +~ pt, r, c)
