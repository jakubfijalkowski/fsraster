module FsRaster.D3.Renderer

open System.Diagnostics.CodeAnalysis
open FSharp.Collections.ParallelSeq

open FsRaster
open FsRaster.Utils
open FsRaster.PixelArray
open FsRaster.D3.Math
open FsRaster.D3.Camera
open FsRaster.D3.Light
open FsRaster.D3.Model

type Renderer3D =
    {
        Model : Matrix4;
        Projection : Matrix4;

        Camera : Camera;
        Wireframe : bool;

        BackfaceCulling : bool;

        LightEnabled : bool;
        Light : Light;

        ZBufferEnabled : bool;
        Width : int;
        Height : int;
    }

[<Literal>]
let DefaultFoV = 60.0
[<Literal>]
let NearPlane = 0.1
[<Literal>]
let FarPlane = 100.0
[<Literal>]
let WireframeColor = 0xffffffff

let zBufferMaxValue = double System.Int32.MaxValue - 1.0

let defaultRenderer =
    {
        Model = matIdentity;
        Projection = matIdentity;

        Camera = defaultCamera;
        Wireframe = true;

        BackfaceCulling = false;

        LightEnabled = false;
        Light = defaultLight;

        ZBufferEnabled = false;
        Width = 1;
        Height = 1;
    }

let inline toggleWireframe renderer =
    { renderer with Wireframe = not renderer.Wireframe }

let inline toggleBackfaceCulling renderer =
    { renderer with BackfaceCulling = not renderer.BackfaceCulling }

let inline toggleZBuffer renderer =
    { renderer with ZBufferEnabled = not renderer.ZBufferEnabled }

let inline toggleLight renderer =
    { renderer with LightEnabled = not renderer.LightEnabled }

let setCameraTo camera renderer =
    let cam = updateMatrix camera
    { renderer with Camera = cam }

let inline setLightTo light renderer =
    { renderer with Light = light }

let updateSize renderer width height =
    let aspect = double width / double height
    let newProj = matProjection DefaultFoV aspect NearPlane FarPlane
    { renderer with Projection = newProj; Width = width; Height = height }

let inline private toEyeSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> renderer.Camera.View * renderer.Model * v)
    { model with Vertices = newVerts }

let inline private toClipSpace renderer model =
    let newVerts = model.Vertices |> Array.map (fun v -> (renderer.Projection * v).Normalized)
    { model with Vertices = newVerts }

let inline private toScreenSpace w h model =
    let newVerts = model.Vertices |> Array.map (fun v ->
        // See renderer.c for a better explanation on z-coordinate
        vec4 ((v.X + 1.0) / 2.0 * w) ((-v.Y + 1.0) / 2.0 * h) (zBufferMaxValue - (v.Z + 1.0) / 2.0 * zBufferMaxValue) 1.0
    )
    { model with Vertices = newVerts }

let private calculateLightning renderer model =
    let calculatePhong i c =
        let v = toVec3 model.Vertices.[i]
        let n = model.Normals.[i]    

        let l = (renderer.Light.Position - v).Normalized
        let dotLN = dot3 l n
        let r = 2.0 * dotLN * n - l
        let viewer = (renderer.Camera.Position - v).Normalized
        let dotRV = dot3 r viewer

        let dCoeff = max 0.0 dotLN
        let sCoeff = System.Math.Pow(max 0.0 dotRV, model.Material.Shininess)

        let aR = int (model.Material.AmbientCoeff * renderer.Light.AmbientR)
        let aG = int (model.Material.AmbientCoeff * renderer.Light.AmbientG)
        let aB = int (model.Material.AmbientCoeff * renderer.Light.AmbientB)
        let dR = int (model.Material.DiffuseCoeff * renderer.Light.DiffuseR * dCoeff)
        let dG = int (model.Material.DiffuseCoeff * renderer.Light.DiffuseG * dCoeff)
        let dB = int (model.Material.DiffuseCoeff * renderer.Light.DiffuseB * dCoeff)
        let sR = int (model.Material.SpecularCoeff * renderer.Light.SpecularR * sCoeff)
        let sG = int (model.Material.SpecularCoeff * renderer.Light.SpecularG * sCoeff)
        let sB = int (model.Material.SpecularCoeff * renderer.Light.SpecularB * sCoeff)
        let r = Colors.clamp (aR + dR + sR + Colors.getR c)
        let g = Colors.clamp (aG + dG + sG + Colors.getG c)
        let b = Colors.clamp (aB + dB + sB + Colors.getB c)
        Colors.fromRGB r g b

    let newColors = model.Colors |> Array.mapi calculatePhong
    { model with Colors = newColors }

let inline private isInView (v : Vector4) =
    v.X >= -1.0 && v.X <= 1.0 && v.Y >= -1.0 && v.Y <= 1.0 && v.Z >= -1.0 && v.Z <= 1.0

let private cullModelToFrustum model = 
    let newTris =
        model.Triangles
        |> Array.filter(fun t ->
            let v1 = model.Vertices.[t.V1]
            let v2 = model.Vertices.[t.V2]
            let v3 = model.Vertices.[t.V3]
            (isInView v1 || isInView v2 || isInView v3)
                && (v1.Z >= -1.0 && v1.Z <= 1.0 && v2.Z >= -1.0 && v2.Z <= 1.0 && v3.Z >= -1.0 && v3.Z <= 1.0)
        )
    { model with Triangles = newTris }

let private cullBackfaces model =
    let newTris =
        model.Triangles
        |> Array.filter (fun t ->
            let v1 = model.Vertices.[t.V1] |> toVec3
            let v2 = model.Vertices.[t.V2] |> toVec3
            let v3 = model.Vertices.[t.V3] |> toVec3
            let n = computeNormal3 v1 v3 v2
            dot3 v1 n >= 0.0
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

let transformModel renderer w h model =
    model
    |> if renderer.LightEnabled then calculateLightning renderer else id
    |> toEyeSpace renderer
    |> if renderer.BackfaceCulling then cullBackfaces else id
    |> toClipSpace renderer
    |> cullModelToFrustum
    |> toScreenSpace w h