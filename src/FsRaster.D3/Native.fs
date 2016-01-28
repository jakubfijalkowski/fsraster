namespace FsRaster.D3

open System
open System.Runtime.InteropServices

open FsRaster.D3.Math
open FsRaster.D3.Model

#nowarn "9"

[<RequireQualifiedAccess>]
module Native =

    [<StructLayout(LayoutKind.Sequential)>]
    type Vector3Native =
        struct
            val X : double
            val Y : double
            val Z : double
            new(x : double, y : double, z : double) = { X = x; Y = y; Z = z }
        end

    let inline toVecNative (v : Vector4) = Vector3Native(v.X, v.Y, v.Z)

    [<StructLayout(LayoutKind.Sequential)>]
    type RenderTriangle =
        struct
            val V1 : Vector3Native
            val V2 : Vector3Native
            val V3 : Vector3Native
            val C1 : int
            val C2 : int
            val C3 : int
            new(v1 : Vector3Native, v2 : Vector3Native, v3 : Vector3Native, c1 : int, c2 : int, c3 : int) =
                { V1 = v1; V2 = v2; V3 = v3; C1 = c1; C2 = c2; C3 = c3 }
        end

    let inline toRenderTriangle model (t : Triangle) : RenderTriangle =
        let v1 = toVecNative model.Vertices.[t.V1]
        let v2 = toVecNative model.Vertices.[t.V2]
        let v3 = toVecNative model.Vertices.[t.V3]
        let c1 = model.Colors.[t.C1]
        let c2 = model.Colors.[t.C2]
        let c3 = model.Colors.[t.C3]
        RenderTriangle(v1, v2, v3, c1, c2, c3)

    [<DllImport("FsRaster.Native.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern void render_triangles(int width, int height, bool zBuffer, int *screen, RenderTriangle *triangles, int count)

    let inline pinAndRender width height zbuffer screen (tris : RenderTriangle array) = 
        let handle = GCHandle.Alloc(tris, GCHandleType.Pinned)
        let ptr = handle.AddrOfPinnedObject() |> NativeInterop.NativePtr.ofNativeInt
        render_triangles(width, height, zbuffer, screen, ptr, tris.Length)
        handle.Free()
