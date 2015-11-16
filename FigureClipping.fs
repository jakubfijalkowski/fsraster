module FsRaster.FigureClipping

open FSharp.Collections.ParallelSeq

open FsRaster.Utils
open FsRaster.Figures

let asPolygon c (x1, y1, x2, y2) = Polygon ([(x1, y1); (x2, y1); (x2, y2); (x1, y2)], c)

let private yPointOnLine x (x1, y1) (x2, y2) =
    let a = double (y2 - y1) / double (x2 - x1)
    let b = double y1 - a * double x1
    int <| round (a * double x + b)

let private xPointOnLine y (x1, y1) (x2, y2) =
    if x1 = x2 then x1
    else
        let a = double (y2 - y1) / double (x2 - x1)
        let b = double y1 - a * double x1
        int <| round ((double y - b) / a)

let prepareSutherlandHodgmanTests testGeneric (left, top, right, bottom) =
    let testLeft = testGeneric fst (fun s n -> (left, yPointOnLine left s n)) left
    let testRight = testGeneric (fst >> (~-)) (fun s n -> (right, yPointOnLine right s n)) -right
    let testTop = testGeneric snd (fun s n -> (xPointOnLine top s n, top)) top
    let testBottom = testGeneric (snd >> (~-)) (fun s n -> (xPointOnLine bottom s n, bottom)) -bottom
    List.map uncurry [testLeft; testTop; testRight; testBottom]

let private clipLine p1 p2 clipRect =
    let testGeneric item crossPoint bound s n =
        match (item s, item n) with
        | (a, b) when a <= bound && b <= bound -> []
        | (a, b) when a >  bound && b >  bound -> [s; n]
        | (a, b) when a <= bound && b >  bound -> [crossPoint s n; n]
        | (a, b) when a >  bound && b <= bound -> [s; crossPoint s n]
        | _ -> []
    let tests = prepareSutherlandHodgmanTests testGeneric clipRect
    let result = tests |> List.fold (fun pts t -> List.pairwise pts |> List.collect t) [p1; p2]
    if List.isEmpty result then None else Some (List.head result, List.last result)

let private clipPolygon pts clipRect =
    let testGeneric item crossPoint bound s n =
        match (item s, item n) with
        | (a, b) when a >= bound && b >= bound -> [n]
        | (a, b) when a <  bound && b =  bound -> [n]
        | (a, b) when a <= bound && b <  bound -> []
        | (a, b) when a <  bound && b >  bound -> [crossPoint s n; n]
        | (a, b) when a >  bound && b <  bound -> [crossPoint s n] 
        | _ -> []
    let tests = prepareSutherlandHodgmanTests testGeneric clipRect
    let result = tests |> List.fold (fun pts' t -> List.pairwise pts' |> List.collect t |> makeConnected) (makeConnected pts)
    if List.isEmpty result then None else Some (List.butLast result)

let clipFigure clipRect = function
    | Line (p1, p2, c) -> Option.map (fun (p1', p2') -> Line (p1', p2', c)) (clipLine p1 p2 clipRect)
    | Polygon (pts, c) -> Option.map (fun p -> Polygon (p, c)) (clipPolygon pts clipRect)
    | r -> Some r

let clipFigures (figs : Figure seq) clipRect : Figure seq = PSeq.choose (clipFigure clipRect) figs |> PSeq.toList :> Figure seq
