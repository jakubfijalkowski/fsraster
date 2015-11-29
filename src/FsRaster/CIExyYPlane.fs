module FsRaster.CIExyYPlane

open System.IO

open FSharp.Data
open FSharp.Collections.ParallelSeq

open FsRaster.Resources
open FsRaster.FigureRendering

[<Literal>]
let private DataFileName = "CIEXYZ_color_matching_functions"

type CIEXYZMatchingFunctions = CsvProvider<"CIEXYZ_color_matching_functions.csv">

let colorMatchingFunctions =
    use stream = loadRawStream DataFileName :> Stream
    CIEXYZMatchingFunctions.Load(stream)

let xySpectrumLocusPoints =
    colorMatchingFunctions.Rows |> Seq.toArray |> Array.map (fun v ->
        let sum = v.X + v.Y + v.Z
        (double (v.X / sum), double (v.Y / sum))
    )

let renderxyHorseshoe width' height' =
    let width = double width'
    let height = double height'
    let color = FigureColor.Color 0xff000000
    let points =
        xySpectrumLocusPoints
        |> Array.map (fun (x, y) -> (int <| x * width, int <| y * height))
        |> Array.toList
    renderFilledPolygonSolid points color |> PSeq.toArray
    