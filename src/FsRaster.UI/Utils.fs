namespace FsRaster.UI.Utils

open System.Windows
open System.Windows.Data

open FsRaster.Figures

module UIUtils =
    let private toString (a : 'a) = sprintf "%A" a
    let private getApproxLocation = function
    | Point (p, _)                -> [ toString p ]
    | Line (p1, p2, _)            -> [ toString p1; toString p2 ]
    | Circle (p, r, _)            -> [ toString p; toString r ]
    | AntialiasedCircle (p, r, _) -> [ toString p; toString r ]
    | SquareLine (p1, p2, _, _)   -> [ toString p1; toString p2 ]
    | DiamondLine (p1, p2, _, _)  -> [ toString p1; toString p2 ]
    | CircleLine (p1, p2, _, _)   -> [ toString p1; toString p2 ]
    | Polyline (pts, _)           -> [ toString <| List.minBy fst pts ]
    | Polygon (pts, _)            -> [ toString <| List.minBy fst pts ]
    | FilledPolygon (pts, _)      -> [ toString <| List.minBy fst pts ]
    | Brush (pts, _)              -> [ toString <| List.minBy fst pts ]
    | FilledBrush (pts, _)        -> [ toString <| List.minBy fst pts ]
    let private getApproxLocationString fig =
        let loc = getApproxLocation fig
        match loc with
        | [ p ] -> p
        | [ p1; p2 ] -> sprintf "(%s, %s)" p1 p2
        | _ -> ""

    let private getThickness = function
    | SquareLine (_, _, _, t)  -> t
    | DiamondLine (_, _, _, t) -> t
    | CircleLine (_, _, _, t)  -> t
    | _                        -> 0
    let private getThicknessString fig =
        let th = getThickness fig
        if th > 0 then sprintf " (Thickness: %d)" th else ""

    let private colorName = function
        | FsRaster.FigureColor.Color c -> sprintf "%A" c
        | FsRaster.FigureColor.Texture _ -> "Textured"
        | FsRaster.FigureColor.ReducedTexture _ -> "Textured-Reduced"

    let private figureName fig = fig.GetType().Name
    let shortDescriptionOf fig = (figureName fig).ToLower()
    let longDescriptionOf fig =
        let desc = getFigureInfo fig
        let loc = getApproxLocationString fig
        let thickness = getThicknessString fig
        sprintf "%s %s %s%s" (figureName fig) loc (colorName desc.Color) thickness

type FigureNameConverter() =
    interface IValueConverter with
        member this.Convert(value, _, _, _) = UIUtils.longDescriptionOf (value :?> Figure) :> obj
        member this.ConvertBack(_, _, _, _) = failwith "Operation not supported."