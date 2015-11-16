module FsRaster.FigureBuilding

open FsRaster.Figures

type FigureBuilder = {
    Builder : FigureInfo -> Point list -> Figure;
    PointsLeft : int;
    Points : Point list;
    Preview : Point list -> FigureInfo -> Figure option
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
let private withAtLeast2 e f = function
    | _ :: _ :: _ as pts -> f pts
    | _                  -> e "Invalid number of elements."
let private withAtLeast2' f pts = withAtLeast2 failwith f pts

let private buildLine' c             = withPair' (fun p1 p2 -> (p1, p2, c.Color))
let private buildPoint c             = withSingle' (fun p -> Point (p, c.Color))
let private buildLine c              = buildLine' c >> Line
let private buildCircle c            = withPair' (fun p1 p2 -> Circle (p1, distance p1 p2, c.Color))
let private buildAACircle c          = withPair' (fun p1 p2 -> AntialiasedCircle (p1, distance p1 p2, c.Color))
let private buildSquareLine c        = buildLine' c >> setThickness c >> SquareLine
let private buildDiamondLine c       = buildLine' c >> setThickness c >> DiamondLine
let private buildCircleLine c        = buildLine' c >> setThickness c >> CircleLine
let private buildPolyline c pts      = Polyline (pts, c.Color)
let private buildPolygon c pts       = Polygon (pts, c.Color)
let private buildFilledPolygon c pts = FilledPolygon (pts, c.Color)

let private withPairOpt f pts      = withPair (fun _ -> None) (fun a -> f a >> Some) pts

let private previewBuilder b pts c = withPairOpt (fun _ _ -> b c pts) pts
let private previewCircle pts c    = withPairOpt (fun p1 p2 -> Circle (p1, distance p1 p2, c.Color)) pts
let private previewAACircle pts c  = withPairOpt (fun p1 p2 -> AntialiasedCircle (p1, distance p1 p2, c.Color)) pts
let private previewMultipointBuilder b pts c = withAtLeast2 (fun _ -> None) (b c >> Some) pts

let getFigureBuilder = function
    | Point _             -> { Builder = buildPoint;         PointsLeft = 1; Points = []; Preview = fun _ _ -> None                        }
    | Line _              -> { Builder = buildLine;          PointsLeft = 2; Points = []; Preview = previewBuilder buildLine               }
    | Circle _            -> { Builder = buildCircle;        PointsLeft = 2; Points = []; Preview = previewCircle                          }
    | AntialiasedCircle _ -> { Builder = buildAACircle;      PointsLeft = 2; Points = []; Preview = previewAACircle                        }
    | SquareLine _        -> { Builder = buildSquareLine;    PointsLeft = 2; Points = []; Preview = previewBuilder buildSquareLine         }
    | DiamondLine _       -> { Builder = buildDiamondLine;   PointsLeft = 2; Points = []; Preview = previewBuilder buildDiamondLine        }
    | CircleLine _        -> { Builder = buildCircleLine;    PointsLeft = 2; Points = []; Preview = previewBuilder buildCircleLine         }
    | Polyline _          -> { Builder = buildPolyline;      PointsLeft = 0; Points = []; Preview = previewMultipointBuilder buildPolyline }
    | Polygon _           -> { Builder = buildPolygon;       PointsLeft = 0; Points = []; Preview = previewMultipointBuilder buildPolyline }
    | FilledPolygon _     -> { Builder = buildFilledPolygon; PointsLeft = 0; Points = []; Preview = previewMultipointBuilder buildPolyline }

let processBuildingFigure builder pt c =
    let areTheSame = Option.fold (fun _ p2 -> pt = p2) false (List.tryLast builder.Points)
    if areTheSame then Choice2Of2 builder
    else
        let points = builder.Points @ [pt]
        if builder.PointsLeft = 1
        then Choice1Of2 (builder.Builder c points)
        else Choice2Of2 { builder with Points = points; PointsLeft = builder.PointsLeft - 1 }

let forceFinishFigure builder c =
    if builder.PointsLeft < -1
    then Some (builder.Builder c builder.Points)
    else None

let previewFigure pt c { Preview = prev; Points = pts } = prev (pts @ [pt]) c