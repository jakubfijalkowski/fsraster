﻿module FsRaster.FigureRendering

open System.Collections.Generic
open System.Windows.Media

open FSharp.Collections.ParallelSeq

open FsRaster.Utils
open FsRaster.Figures

let private colorize c lst = PSeq.map (fun a -> (a, c)) lst
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

let private renderPolyline (pts, c) = pts |> List.pairwise |> PSeq.collect (sortPairOfPoints >> uncurry renderLine') |> colorize c
let private renderPolygon (pts, c) = renderPolyline (makeConnected pts, c)

type ActiveEdge = int * double * double


let private getEdges (dict : Dictionary<int, ActiveEdge list>) e =
    match dict.TryGetValue e with
    | true, lst -> lst
    | _         -> []

let private withEdgeList (dict : Dictionary<int, ActiveEdge list>) e f =
    let oldList = getEdges dict e
    dict.[e] <- f oldList

let private buildEdgeTable pts =
    let edgeTable = new Dictionary<int, ActiveEdge list>()
    let edges = List.pairwise (makeConnected pts) |> List.filter (fun ((_, y1), (_, y2)) -> y1 <> y2)
    for ((sx, sy), (ex, ey)) in edges do
        let xmin, ymin = if sy < ey then (sx, sy) else (ex, ey)
        let ymax = max sy ey
        let coeff = double (sx - ex) / double (sy - ey) 
        withEdgeList edgeTable ymin (curry List.Cons (ymax, double xmin, coeff))
    edgeTable

let rec inPairs = function
    | a :: b :: rest -> (a, b) :: inPairs rest
    | _              -> []

let private renderFilledPolygon (pts, c) =
    let edgeTable = buildEdgeTable pts
    let _, ymin = List.minBy snd pts
    let _, ymax = List.maxBy snd pts
    let processSingleScanline aet y =
        let aet' = aet @ getEdges edgeTable y |> List.sortBy (fun (_, x, _) -> x)
        let pixels = aet' |> inPairs |> List.collect (fun ((_, x1, _), (_, x2, _)) -> [ int (round x1) .. int (round x2) ])
        let pixels' = pixels |> List.map (fun x -> (x, y))
        let aet' = aet' |> List.filter (fun (ymax, _, _) -> ymax <> y + 1) |> List.map (fun (ymax, x, coeff) -> (ymax, x + coeff, coeff))
        (pixels', aet')
    let processSingle (pixels, aet) y =
        let (pixels', aet') = processSingleScanline aet y
        (Seq.append pixels pixels', aet')
    [ ymin .. ymax ] |> List.fold processSingle (Seq.empty, []) |> fst |> colorize c

let renderSingleFigure = function
    | Point  p            -> renderPoint p
    | Line   a            -> renderLine a
    | Circle c            -> renderCircle c
    | AntialiasedCircle c -> renderAACircle c
    | SquareLine s        -> renderSquareLine s
    | DiamondLine s       -> renderDiamondLine s
    | CircleLine s        -> renderCircleLine s
    | Polyline p          -> renderPolyline p
    | Polygon p           -> renderPolygon p
    | FilledPolygon p     -> renderFilledPolygon p

let renderFigureList figs = PSeq.ordered figs |> PSeq.map renderSingleFigure |> PSeq.toList
