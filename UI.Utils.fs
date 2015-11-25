namespace FsRaster.UIUtils

open System.Windows
open System.Windows.Data

open FsRaster.Figures

type FigureNameConverter() =
    interface IValueConverter with
        member this.Convert(value, _, _, _) = longDescriptionOf (value :?> Figure) :> obj
        member this.ConvertBack(_, _, _, _) = failwith "Operation not supported."