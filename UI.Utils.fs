namespace FsRaster.UI.Utils

open System.Windows
open System.Windows.Data

open FsRaster.Figures

module UIUtils = 
    let private colorName = function
        | FsRaster.FigureColor.Color c -> sprintf "Color: %A" c
        | FsRaster.FigureColor.BitmapPattern _ -> "Texture"

    let private figureName fig = fig.GetType().Name
    let shortDescriptionOf fig = (figureName fig).ToLower()
    let longDescriptionOf fig =
        let desc = getFigureInfo fig
        sprintf "%s %s" (figureName fig) (colorName desc.Color)

type FigureNameConverter() =
    interface IValueConverter with
        member this.Convert(value, _, _, _) = UIUtils.longDescriptionOf (value :?> Figure) :> obj
        member this.ConvertBack(_, _, _, _) = failwith "Operation not supported."