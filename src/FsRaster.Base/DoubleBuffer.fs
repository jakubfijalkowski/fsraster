module FsRaster.DoubleBuffer

open System.Runtime.InteropServices

open Microsoft.FSharp.NativeInterop

open FsRaster.Utils.Native
open FsRaster.RawRendering

#nowarn "9"

let inline private allocPixelArray w h =
    Marshal.AllocHGlobal(w * h * sizeof<int>) |> NativePtr.ofNativeInt

let inline private freePixelArray ptr = Marshal.FreeHGlobal (NativePtr.toNativeInt ptr)

type DoubleBuffer =
    {
        mutable FrontBuffer : int nativeptr;
        mutable BackBuffer : int nativeptr;
        mutable Width : int;
        mutable Height : int

        FrontLock : obj;
        BackLock : obj;
    }

let makeDoubleBuffer width height =
    let front = allocPixelArray width height
    let back = allocPixelArray width height
    { FrontBuffer = front; BackBuffer = back; Width = width; Height = height; FrontLock = obj(); BackLock = obj() }

let inline withBackBuffer buffer f = lock buffer.BackLock (fun () -> f buffer.BackBuffer)
let inline withFrontBuffer buffer f = lock buffer.FrontLock (fun () -> f buffer.FrontBuffer)
let inline withBothBuffers buffer f =
    lock buffer.BackLock (fun () ->
        lock buffer.FrontLock (fun () ->
            f buffer.BackBuffer buffer.FrontBuffer
        )
    )

let inline updateAndSwap buffer action =
    withBackBuffer buffer (fun back ->
        action { Pixels = back; Width = buffer.Width; Height = buffer.Height }
        withFrontBuffer buffer (fun front ->
            buffer.BackBuffer <- front
            buffer.FrontBuffer <- back
        )
    )

let inline showOnScreen buffer bmp =
    withFrontBuffer buffer (fun front ->
        let len = buffer.Width * buffer.Height
        copyMemory front 0 bmp 0 len
    )

let updateBufferSize buffer width height =
    withBothBuffers buffer (fun back front ->
        freePixelArray front
        freePixelArray back
        buffer.Width <- width
        buffer.Height <- height
        buffer.BackBuffer <- allocPixelArray width height
        buffer.FrontBuffer <- allocPixelArray width height
    )
