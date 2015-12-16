﻿namespace FsRaster.UI

open System.Windows.Media.Imaging
open System.Windows.Controls

open FsRaster.CoreRendering

type HistogramControl() as self =
    inherit UserControl()

    let bitmap = BitmapFactory.New(256, 256)

    do
        let img = new Image()
        img.Source <- bitmap
        self.Content <- img

    member this.UpdateHistogram data c =
        let scale = data |> Array.max |> double
        use bitmapCtx = bitmap.GetBitmapContext(ReadWriteMode.ReadWrite)
        bitmapCtx.Clear()
        let ctx = { Width = bitmap.PixelWidth; Height = bitmap.PixelHeight; Context = bitmapCtx }
        for x in 0 .. 255 do
            if data.[x] > 0 then
                let height = int (255.0 * (double data.[x] / scale))
                for y in 255 - height .. 255 do
                    putPixel ctx x y c