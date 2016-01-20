module FsRaster.D3.Renderer

open System.Diagnostics.CodeAnalysis
open FSharp.Collections.ParallelSeq

open FsRaster
open FsRaster.Utils
open FsRaster.D3.Math
open FsRaster.D3.Models
open FsRaster.D3.Camera

type Renderer3D =
    {
        Model : Matrix4;
        Projection : Matrix4;
        Cumulative : Matrix4;
        Camera : Camera;
        Wireframe : bool
    }

[<Literal>]
let NearPlane = 0.1
[<Literal>]
let FarPlane = 100.0
[<Literal>]
let WireframeColor = 0xffffffff

let defaultRenderer =
    {
        Model = matIdentity;
        Projection = matIdentity;
        Cumulative = matIdentity;
        Camera = defaultCamera;
        Wireframe = true
    }

let setCameraTo camera renderer =
    let cam = updateMatrix camera
    let cumulative = renderer.Projection * cam.View * renderer.Model
    { renderer with Camera = cam; Cumulative = cumulative }

let updateProjection renderer width height =
    let aspect = double width / double height
    let newProj = matProjection 90.0 aspect NearPlane FarPlane
    let cumulative = newProj * renderer.Camera.View * renderer.Model
    { renderer with Projection = newProj; Cumulative = cumulative }

let inline toggleWireframe renderer =
    { renderer with Wireframe = not renderer.Wireframe }

let private toClipSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> (renderer.Cumulative * v).Normalized)
    { model with Vertices = newVerts }

let private toScreenSpace w h model =
    let newVerts = model.Vertices |> Array.map (fun v ->
        vec4 ((v.X + 1.0) / 2.0 * w) ((-v.Y + 1.0) / 2.0 * h) v.Z 1.0
    )
    { model with Vertices = newVerts }

let inline private isInView (v : Vector4) =
    v.X >= -1.0 && v.X <= 1.0 && v.Y >= -1.0 && v.Y <= 1.0 && v.Z >= -1.0 && v.Z <= 1.0

let private clipModel model =
    let newTris =
        model.Triangles
        |> Array.filter(fun (a, b, c) ->
            let va = model.Vertices.[a]
            let vb = model.Vertices.[b]
            let vc = model.Vertices.[c]
            isInView va && isInView vb && isInView vc
        )
    { model with Triangles = newTris }

let renderWireframe render model =
    let renderTriangle (a, b, c) =
        let v1 = model.Vertices.[a]
        let v2 = model.Vertices.[b]
        let v3 = model.Vertices.[c]
        render v1 v2 WireframeColor
        render v1 v3 WireframeColor
        render v2 v3 WireframeColor
    Array.iter renderTriangle model.Triangles

let transformModel renderer w h model =
    model |> toClipSpace renderer |> clipModel |> toScreenSpace w h
