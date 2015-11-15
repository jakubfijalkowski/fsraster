module FsRaster.FigureHitTests

open FsRaster.Figures

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
    | SquareLine (p1, p2, c, _)  -> isLineHit pt (p1, p2, c)
    | DiamondLine (p1, p2, c, _) -> isLineHit pt (p1, p2, c)
    | CircleLine (p1, p2, c, _)  -> isLineHit pt (p1, p2, c)
