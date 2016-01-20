module FsRaster.D3.Models

open System
open System.Globalization
open System.IO
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

open FsRaster
open FsRaster.D3.Math

type Model =
    {
        Vertices : Vector4 array;
        Triangles : (int * int * int) array;
    }

let sampleTriangle =
    {
        Vertices = [| vec4 0.0 0.0 0.0 1.0; vec4 0.0 0.5 0.0 1.0; vec4 0.5 0.0 0.0 1.0 |];
        Triangles = [| 0, 1, 2 |]
    }

let private readAllLines (stream : Stream) =
    let lines = new List<string>()
    use reader = new StreamReader(stream)
    while not reader.EndOfStream do
        let line = reader.ReadLine()
        if not <| System.String.IsNullOrWhiteSpace line then lines.Add(line.Trim())
    lines.ToArray()

let private parseVertex (line : string) =
    let components = line.Split(' ') |> Array.map (fun s -> Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture))
    if components.Length <> 3 then failwith "Invalid number of components in vertex"
    if components |> Array.map fst |> Array.fold (&&) true |> not then failwith "Could not parse vertex"
    let coords = components |> Array.map snd
    vec4 coords.[0] coords.[1] coords.[2] 1.0

let private parseFace (line : string) =
    let components = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.TryParse
    if components.Length <> 4 then failwith "Invalid number of components in face definition"
    if components |> Array.map fst |> Array.fold (&&) true |> not then failwith "Could not parse face"
    let indices = components |> Array.map snd
    if indices.[0] <> 3 then failwith "Only triangles are supported"
    (indices.[1], indices.[2], indices.[3])

[<SuppressMessage("CyclomaticComplexity", "*")>]
let loadOffFromStream (stream : Stream) =
    let lines = readAllLines stream |> Array.filter (fun l -> not <| l.StartsWith("#"))
    if lines.Length < 2 then failwith "Invalid format - no header/info line"

    let header = lines.[0]
    if header <> "OFF" then failwith "Invalid header"

    let info = lines.[1].Split(' ')
    if info.Length <> 3 || info.[2] <> "0" then failwith "Invalid info line"

    let _, vertCount = Int32.TryParse(info.[0])
    let _, faceCount = Int32.TryParse(info.[1])
    if vertCount = 0 || faceCount = 0 then failwith "Invalid info line or empty model"

    if lines.Length <> vertCount + faceCount + 2 then failwith "Invalid number of lines"

    let vertices = lines |> Array.skip 2 |> Array.take vertCount |> Array.map parseVertex
    let triangles = lines |> Array.skip (2 + vertCount) |> Array.map parseFace

    if triangles |> Array.tryFind (fun (a, b, c) -> a < 0 || a >= vertCount || b < 0 || b >= vertCount || c < 0 || c >= vertCount) |> Option.isSome then
        failwith "Invalid index detected"

    { Vertices = vertices; Triangles = triangles }

let loadOffFromResources name =
    use stream = Resources.loadStream name
    loadOffFromStream stream