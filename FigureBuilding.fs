﻿module FsRaster.FigureBuilding

open FsRaster.Figures

type FigureBuilder = {
    Builder : FigureInfo -> Point list -> Figure;
    PointsLeft : int;
    Points : Point list;
    Preview : Point list -> FigureInfo -> Figure seq
}

let private setThickness i (p1, p2, c) = (p1, p2, c, i.Thickness)

let private withSingle e f = function
    | [p1] -> f p1
    | _    -> e "Invalid number of elements"
let private withSingle' =  withSingle failwith
let private withPair e f = function
    | [p1; p2] -> f p1 p2
    | _        -> e "Invalid number of elements."
let private withPair' f pts = withPair failwith f pts
let private sortPoints (pts : Point list) = List.sortBy fst pts

let private buildLine' c = sortPoints >> withPair' (fun p1 p2 -> (p1, p2, c.Color))
let private buildPoint c = withSingle' (fun p -> Point (p, c.Color))
let private buildLine c = buildLine' c >> Line
let private buildCircle c = withPair' (fun p1 p2 -> Circle (p1, distance p1 p2, c.Color))
let private buildAACircle c = withPair' (fun p1 p2 -> AntialiasedCircle (p1, distance p1 p2, c.Color))
let private buildSquareLine c = buildLine' c >> setThickness c >> SquareLine
let private buildDiamondLine c = buildLine' c >> setThickness c >> DiamondLine
let private buildCircleLine c = buildLine' c >> setThickness c >> CircleLine

let private withPairSeq f pts = withPair (fun _ -> Seq.empty) (fun a -> f a >> Seq.singleton) pts
let private previewBuilder b pts c = withPairSeq (fun _ _ -> b c pts) pts
let private previewCircle pts c = withPairSeq (fun p1 p2 -> Circle (p1, distance p1 p2, c.Color)) pts
let private previewAACircle pts c = withPairSeq (fun p1 p2 -> AntialiasedCircle (p1, distance p1 p2, c.Color)) pts

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

let forceFinishFigure builder c =
    if builder.PointsLeft < 0
    then Some (builder.Builder c builder.Points)
    else None

let previewFigure ({ Preview = prev; Points = pts } as builder) pt c = prev (pts @ [pt]) c