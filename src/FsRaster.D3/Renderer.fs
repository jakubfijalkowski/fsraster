module FsRaster.D3.Renderer

open System.Diagnostics.CodeAnalysis
open FSharp.Collections.ParallelSeq

open FsRaster
open FsRaster.Utils
open FsRaster.D3.Math
open FsRaster.D3.Camera
open FsRaster.D3.Light
open FsRaster.D3.Model

type Renderer3D =
    {
        Model : Matrix4;
        Projection : Matrix4;
        InvProjection : Matrix4;

        Camera : Camera;
        Wireframe : bool;

        FrustumCulling : bool;
        BackfaceCulling : bool;

        LightEnabled : bool;
        Light : Light;

        ZBufferEnabled : bool;
        Width : int;
        Height : int;
        ZBuffer : double array // using option would complicate already complicated code in low-level rendering
    }

[<Literal>]
let DefaultFoV = 60.0
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
        InvProjection = matIdentity;

        Camera = defaultCamera;
        Wireframe = true;

        FrustumCulling = false;
        BackfaceCulling = false;

        LightEnabled = false;
        Light = defaultLight;

        ZBufferEnabled = false;
        Width = 1;
        Height = 1;
        ZBuffer = null
    }

let inline private updateZBuffer renderer =
    if renderer.ZBufferEnabled then
        let newBuffer = Array.zeroCreate (renderer.Width * renderer.Height)
        { renderer with ZBuffer = newBuffer }
    else { renderer with ZBuffer = null }

let inline toggleWireframe renderer =
    { renderer with Wireframe = not renderer.Wireframe }

let inline toggleFrustumCulling renderer =
    { renderer with FrustumCulling = not renderer.FrustumCulling }

let inline toggleBackfaceCulling renderer =
    { renderer with BackfaceCulling = not renderer.BackfaceCulling }

let inline toggleZBuffer renderer =
    { renderer with ZBufferEnabled = not renderer.ZBufferEnabled } |> updateZBuffer

let inline toggleLight renderer =
    { renderer with LightEnabled = not renderer.LightEnabled }

let setCameraTo camera renderer =
    let cam = updateMatrix camera
    { renderer with Camera = cam }

let updateSize renderer width height =
    let aspect = double width / double height
    let newProj = matIdentity
    let newInvProj = matIdentity
    let newProj = matProjection DefaultFoV aspect NearPlane FarPlane
    let newInvProj = matInvProjection DefaultFoV aspect NearPlane FarPlane
    { renderer with Projection = newProj; InvProjection = newInvProj; Width = width; Height = height } |> updateZBuffer

let inline private toEyeSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> renderer.Camera.View * renderer.Model * v)
    { model with Vertices = newVerts }

let inline private toClipSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> (renderer.Projection * v).Normalized)
    { model with Vertices = newVerts }

let private normalsToScreenSpace renderer model =
    let newNormals = model.Normals |> Array.map (fun v -> toVec3 (renderer.InvProjection * renderer.Camera.View * toVec4 v))
    { model with Normals = newNormals }

let inline private toScreenSpace w h model =
    let newVerts = model.Vertices |> Array.map (fun v ->
        vec4 ((v.X + 1.0) / 2.0 * w) ((-v.Y + 1.0) / 2.0 * h) ((v.Z + 1.0) / 2.0) 1.0
    )
    { model with Vertices = newVerts }

let inline private isInView (v : Vector4) =
    v.X >= -1.0 && v.X <= 1.0 && v.Y >= -1.0 && v.Y <= 1.0 && v.Z >= -1.0 && v.Z <= 1.0

let inline private transformLight w h renderer =
    let newPos = renderer.Projection * renderer.Camera.View * (toVec4 renderer.Light.Position) |> toVec3
    let newX = (newPos.X + 1.0) / 2.0
    let newY = (-newPos.Y + 1.0) / 2.0
    let newZ = (newPos.Z + 1.0) / 2.0
    let newLight = { renderer.Light with Position = vec3 newX newY newZ }
    { renderer with Light = newLight }

let private clipModel model =
    let newTris =
        model.Triangles
        |> Array.filter(fun t ->
            let v1 = model.Vertices.[t.V1]
            let v2 = model.Vertices.[t.V2]
            let v3 = model.Vertices.[t.V3]
            isInView v1 && isInView v2 && isInView v3
        )
    { model with Triangles = newTris }

let private cullBackfaces model =
    let newTris =
        model.Triangles
        |> Array.filter (fun t ->
            let v1 = model.Vertices.[t.V1] |> toVec3
            let v2 = model.Vertices.[t.V2] |> toVec3
            let v3 = model.Vertices.[t.V3] |> toVec3
            let n = computeNormal3 v1 v2 v3
            dot3 (-v1) n >= 0.0
        )
    { model with Triangles = newTris }

let renderWireframe render model =
    let renderTriangle (t : Triangle) =
        let v1 = model.Vertices.[t.V1]
        let v2 = model.Vertices.[t.V2]
        let v3 = model.Vertices.[t.V3]
        render v1 v2 WireframeColor
        render v1 v3 WireframeColor
        render v2 v3 WireframeColor
    Array.iter renderTriangle model.Triangles

let renderFilled render model =
    model.Triangles |> Array.iter (toRenderTriangle model >> render)

let transformModel renderer w h model =
    let model' =
        model
        |> toEyeSpace renderer
        |> if renderer.BackfaceCulling then cullBackfaces else id
        |> toClipSpace renderer
        |> if renderer.FrustumCulling then clipModel else id
        |> toScreenSpace w h
        |> normalsToScreenSpace renderer

    let renderer' =
        if renderer.LightEnabled then
            renderer |> transformLight w h
        else renderer
    (renderer', model')
