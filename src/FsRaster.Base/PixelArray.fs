module FsRaster.PixelArray

open System.Runtime.InteropServices

open Microsoft.FSharp.NativeInterop

#nowarn "9"

let inline nullptr<'a when 'a : unmanaged> : nativeptr<'a> = NativePtr.ofNativeInt (nativeint 0)

let inline allocPixelArray w h : 'a nativeptr =
    Marshal.AllocHGlobal(w * h * sizeof<'a>) |> NativePtr.ofNativeInt

let inline freePixelArray ptr =
    if ptr <> nullptr then Marshal.FreeHGlobal (NativePtr.toNativeInt ptr)

let inline resizePixelArray ptr w h =
    freePixelArray ptr
    allocPixelArray w h