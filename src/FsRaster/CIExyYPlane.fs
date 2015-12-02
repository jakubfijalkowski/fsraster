module FsRaster.CIExyYPlane

open System.IO
open FSharp.Data
open FSharp.Collections.ParallelSeq
open FsRaster.Figures
open FsRaster.Resources
open FsRaster.FigureRendering

[<Literal>]
let private DataFileName = "CIEXYZ_color_matching_functions"

type CIEXYZMatchingFunctions = CsvProvider< "CIEXYZ_color_matching_functions.csv" >

let colorMatchingFunctions = 
    let data = loadString DataFileName
    CIEXYZMatchingFunctions.Parse(data)

let xySpectrumLocusPoints = 
    colorMatchingFunctions.Rows
    |> Seq.toArray
    |> Array.map (fun v -> 
           let sum = v.X + v.Y + v.Z
           (double (v.X / sum), double (v.Y / sum)))

let private fixHorseshoe = 
    let extractCoords = 
        function 
        | PrimLine p -> p
        | _ -> failwith "Should not happen"
    
    let selectSingle (a : (int * 'a * int * 'b) []) = 
        if Array.length a = 1 then a.[0]
        else Array.maxBy (fun (x1, _, x2, _) -> x2 - x1) a
    
    Array.map extractCoords
    >> Array.groupBy (fun (_, y, _, _) -> y)
    >> Array.map (snd >> selectSingle)
    >> Array.map PrimLine

let renderxyHorseshoe width' height' = 
    let width = double width'
    let height = double height'
    let color = FigureColor.Color 0xff000000
    
    let points = 
        xySpectrumLocusPoints
        |> Array.map (fun (x, y) -> (int <| x * width, int <| y * height))
        |> Array.toList
    
    let rendered = renderFilledPolygonSolid points color |> PSeq.toArray
    fixHorseshoe rendered
