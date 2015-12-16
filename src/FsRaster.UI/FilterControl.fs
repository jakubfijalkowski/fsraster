namespace FsRaster.UI

open System

open FSharp.Charting
open FSharp.Charting.ChartTypes
open FSharp.Control.Reactive
open FsXaml

open FsRaster.Utils
open FsRaster.CoreRendering
open FsRaster.Filters

type FilterControl = XAML<"FilterControl.xaml", true>

type FilterControlController(control : FilterControl, rectangle : SceneRectangleController, data : IObservable<IRenderer>, bgColorPicker : FsRaster.UI.ColorPicker.AdvancedColorPicker) =
    let histogramContainer = control.histogramContainer :?> System.Windows.Forms.Integration.WindowsFormsHost

    let getRectangle () =
        Option.map (fun (left, top, right, bottom) -> (left + 1, top + 1, right - 1, bottom - 1)) rectangle.Rectangle
        |> Option.opt (0, 0, 0, 0)

    let getBgColor () = bgColorPicker.SelectedColor |> FsRaster.Colors.fromUIColor

    let getHistogramChannel _ =
        match control.histogramChannel.SelectedIndex with
        | 1 -> FsRaster.Colors.getG
        | 2 -> FsRaster.Colors.getB
        | _ -> FsRaster.Colors.getR

    do
        let histogramChannel =
            control.histogramChannel.SelectionChanged
            |> Observable.map getHistogramChannel
        let histogramData =
            data
            |> Observable.map (fun r -> r.StreamPixels(getRectangle ()))
            |> Observable.combineLatest (fun a b -> (a, b)) histogramChannel
            |> Observable.map (fun (chan, p) -> generateHistogram (getBgColor ()) chan p)
            |> Observable.map (Array.zip [| 0 .. 255 |])
        let chart = LiveChart.Area(histogramData).WithXAxis(Enabled = false, Min = 0.0, Max = 255.0).WithYAxis(Enabled = false)
        histogramContainer.Child <- new ChartControl(chart)

        control.histogramChannel.SelectedIndex <- 0