namespace FsRaster.UI

open System.Collections.Generic

open System.Windows
open System.Windows.Controls
open System.Windows.Media.Imaging

open FsRaster
open FsRaster.Utils
open FsRaster.CoreRendering
open FsRaster.Figures
open FsRaster.FigureRendering

type FunctionDef() as self =
    inherit Image()

    let functionChanged = Event<unit>()

    let bitmap = BitmapFactory.New(256, 256)
    let points = new List<Point>()

    let mutable lineColor = Media.Colors.Black

    let render _ =
        let polyline = points |> Seq.toList |> List.map (second ((~-) >> (+) 255)) // :D

        use ctx = new BitmapRenderer(bitmap.GetBitmapContext(ReadWriteMode.ReadWrite)) :> IRenderer
        ctx.Clear(Media.Colors.White)
        renderFigures ctx [Polyline (polyline, FigureColor.makeColor lineColor)]

    let onMouseDown (e : Input.MouseEventArgs) =
        let pos = e.GetPosition(self)
        let posX, posY = int pos.X, 255 - int pos.Y
        let idx = points.FindIndex(fun (x, _) -> x = posX)
        if idx > -1 then points.[idx] <- (posX, posY)
        else
            points.Add((posX, posY))
            points.Sort(fun (x1, _) (x2, _) -> x1.CompareTo(x2))
        render ()
        functionChanged.Trigger()

    do
        self.ResetPoints()

        render ()

        self.Source <- bitmap
        self.MouseDown.Add onMouseDown

    member this.ResetPoints() =
        points.Clear()
        points.Add((0,0))
        points.Add((255, 255))
        render ()
        functionChanged.Trigger()

    member this.TranslationArray =
        renderPolyline (points |> Seq.toList, FigureColor.Color 0)
        |> Seq.map (fun rp -> match rp with | PrimPixel (p, c) -> p | _ -> (0, 0))
        |> Seq.toArray
        |> Array.sortBy fst
        |> Array.distinctBy fst
        |> Array.map snd

    member this.LineColor
        with get() = lineColor
        and set(value) =
            lineColor <- value
            render ()

    member this.FunctionChanged = functionChanged.Publish