module FsRaster.FigureHitTests

open FsRaster.Utils
open FsRaster.Figures

let private crossProd (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

let private isOnRect (x, y) (x1, y1) (x2, y2) =
    min x1 x2 <= x && x <= max x1 x2 &&
    min y1 y2 <= y && y <= max y1 y2

let private isOnLine p (p1, p2) = distance p1 p + distance p p2 = distance p1 p2

let private doLinesCross (p1, p2) (p3, p4) =
    let d1 = crossProd (p4 -~ p3) (p1 -~ p3)
    let d2 = crossProd (p4 -~ p3) (p2 -~ p3)
    let d3 = crossProd (p2 -~ p1) (p3 -~ p1)
    let d4 = crossProd (p2 -~ p1) (p4 -~ p1)
    let d12 = sign d1 * sign d2
    let d34 = sign d3 * sign d4
    match (d12, d34) with
    | (a, b) when a > 0 || b > 0 -> false
    | (a, b) when a < 0 || b < 0 -> true
    | _                          ->
        isOnRect p1 p3 p4 || isOnRect p2 p3 p4 || isOnRect p3 p1 p2 || isOnRect p4 p1 p2

let private distanceFromLine (x, y) (((x1, y1) as p1), ((x2, y2) as p2)) =
    let d' = max (distance p1 p2) 1
    (abs ((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1)) / d'

let private isPointHit (p1, _) p2 = 
    let d = distance p1 p2
    if d < 10 then Some d else None

let private isLineHit ((x, y) as p) (((x1, y1), (x2, y2)) as l) =
    let d = distanceFromLine p l
    let isInside = x >= x1 && x <= x2 && y >= (min y1 y2) && y <= (max y1 y2)
    if d < 20 && isInside then Some d else None

let private isPolylineHit p pts =
    pts |> List.pairwise |> List.map (sortPairOfPoints >> isLineHit p) |> List.choose id |> List.tryHead

// ignores case when line l contains one of the vertices
let private isPolygonHit p pts =
    let l = (p, first ((+) 9999) p)
    let crossings =
        pts @ [List.head pts]
        |> List.pairwise
        |> List.choose (fun l2 -> if doLinesCross l l2 then Some (distanceFromLine p l2) else None)
    if List.length crossings % 2 = 0 then None else Some (List.min crossings)

let private isCircleHit pt (s, r, _) =
    let d = distance s pt
    if d <= r then Some (r - d) else None

let isFigureHit pt = function
    | Point p                    -> isPointHit p pt
    | Line (p1, p2, _)           -> isLineHit pt (p1, p2)
    | Circle c                   -> isCircleHit pt c
    | AntialiasedCircle c        -> isCircleHit pt c
    | SquareLine (p1, p2, c, _)  -> isLineHit pt (p1, p2)
    | DiamondLine (p1, p2, c, _) -> isLineHit pt (p1, p2)
    | CircleLine (p1, p2, c, _)  -> isLineHit pt (p1, p2)
    | Polyline (pts, _)          -> isPolylineHit pt pts
    | Polygon (pts, _)           -> isPolygonHit pt pts
