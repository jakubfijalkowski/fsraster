module FsRaster.FigureHitTests

open FsRaster.Utils
open FsRaster.Figures

let private isPointHit (p1, _) p2 = 
    let d = distance p1 p2
    if d < 10 then Some d else None

let private isLineHit ((x, y) as p) (((x1, y1), (x2, y2)) as l) =
    let d = distanceFromLine p l
    let isInside = (abs (x1 - x2) = 0 || x >= (min x1 x2) && x <= (max x1 x2)) && (abs (y1 - y2) = 0 || y >= (min y1 y2) && y <= (max y1 y2))
    if d < 20 && isInside then Some d else None

let private isPolylineHit p pts =
    pts |> List.pairwise |> List.map (isLineHit p) |> List.choose id |> List.tryHead

// ignores case when line l contains one of the vertices
let private isPolygonHit p pts =
    let l = (p, first ((+) 9999) p)
    let crossings =
        pts @ [List.head pts]
        |> List.pairwise
        |> List.choose (fun l2 -> if doLinesCross l l2 then Some (distanceFromLine p l2) else None)
    if List.length crossings % 2 = 0 then None else Some 0

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
    | FilledPolygon (pts, _)     -> isPolygonHit pt pts
