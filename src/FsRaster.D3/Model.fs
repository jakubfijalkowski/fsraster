module FsRaster.D3.Model

open System
open System.Globalization
open System.IO
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis

open FsRaster
open FsRaster.D3.Math
open FsRaster.D3.Light

type Triangle = { V1 : int; V2 : int; V3: int; C1 : int; C2 : int; C3 : int }

type RenderTriangle =
    {
        V1 : Vector4; V2 : Vector4; V3 : Vector4;
        C1 : int; C2 : int; C3 : int
    }

type Model =
    {
        Vertices : Vector4 array;
        Normals : Vector3 array;
        Colors : int array;
        Triangles : Triangle array;
        Material : Material
    }

let inline triEmpty v1 v2 v3 : Triangle = { V1 = v1; V2 = v2; V3 = v3; C1 = 0; C2 = 0; C3 = 0 }
let inline toRenderTriangle model (t : Triangle) : RenderTriangle =
    { V1 = model.Vertices.[t.V1]; V2 = model.Vertices.[t.V2]; V3 = model.Vertices.[t.V3];
      C1 = model.Colors.[t.C1] ; C2 = model.Colors.[t.C2] ; C3 = model.Colors.[t.C3]; }

let changeOrientation model =
    let newTriangles = model.Triangles |> Array.map (fun t -> { t with V2 = t.V3; V3 = t.V2 })
    { model with Triangles = newTriangles }

let randomlyColorizeModel model =
    let rnd = System.Random(0xDEADBEFF)
    let genColor _ =
        let r = rnd.Next(50, 256)
        let g = rnd.Next(50, 256)
        let b = rnd.Next(50, 256)
        Colors.fromRGB r g b
    let colors = Array.init model.Vertices.Length genColor
    let triangles = model.Triangles |> Array.map (fun t -> { t with C1 = t.V1; C2 = t.V2; C3 = t.V3 })
    { model with Triangles = triangles; Colors = colors }

let makeItBlack model =
    let colors = Array.create model.Vertices.Length 0xff000000
    let triangles = model.Triangles |> Array.map (fun t -> { t with C1 = t.V1; C2 = t.V2; C3 = t.V3 })
    { model with Triangles = triangles; Colors = colors }

let makeItWhite model =
    let colors = Array.create model.Vertices.Length 0xff000000
    let triangles = model.Triangles |> Array.map (fun t -> { t with C1 = t.V1; C2 = t.V2; C3 = t.V3 })
    { model with Triangles = triangles; Colors = colors }

let private performNormalMapping model =
    let updateNormals (normals : Vector4 array) (t : Triangle) =
        let v1 = model.Vertices.[t.V1]
        let v2 = model.Vertices.[t.V2]
        let v3 = model.Vertices.[t.V3]
        let n = (computeNormal4 v1 v2 v3).Normalized |> toVec4
        normals.[t.V1] <- normals.[t.V1] + n
        normals.[t.V2] <- normals.[t.V2] + n
        normals.[t.V3] <- normals.[t.V3] + n
        normals
    let normals =
        model.Triangles
        |> Array.fold updateNormals (Array.create model.Vertices.Length vec4Zero)
        |> Array.map (fun v -> (toVec3 v).Normalized)
    { model with Normals = normals }

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
    triEmpty indices.[1] indices.[2] indices.[3]

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

    if triangles |> Array.tryFind (fun t -> t.V1 < 0 || t.V1 >= vertCount || t.V2 < 0 || t.V2 >= vertCount || t.V3 < 0 || t.V3 >= vertCount) |> Option.isSome then
        failwith "Invalid index detected"

    { Vertices = vertices; Triangles = triangles; Normals = [||]; Colors = [| 0 |]; Material = defaultMaterial } |> performNormalMapping

let loadOffFromResources name =
    use stream = Resources.loadStream name
    loadOffFromStream stream

let simpleTriangle =
    {
        Vertices = [| vec4Zero; vec4 1.0 0.0 0.0 1.0; vec4 0.0 1.0 0.0 1.0 |];
        Triangles = [|triEmpty 0 1 2|];
        Normals = [||];
        Colors = [| 0xff000000 |];
        Material = defaultMaterial
    } |> performNormalMapping