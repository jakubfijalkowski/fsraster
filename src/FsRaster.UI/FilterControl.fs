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

    let requestRender = Event<unit>()

    let histogramControl = control.histogramControl :?> HistogramControl

    let mutable normalizeHandler = None

    let getRectangle () =
        Option.map (fun (left, top, right, bottom) -> (left + 1, top + 1, right - 1, bottom - 1)) rectangle.Rectangle
        |> Option.opt (0, 0, 0, 0)

    let getBgColor () = bgColorPicker.SelectedColor |> FsRaster.Colors.fromUIColor

    let getHistogramChannel _ =
        match control.histogramChannel.SelectedIndex with
        | 1 -> FsRaster.Colors.getG
        | 2 -> FsRaster.Colors.getB
        | _ -> FsRaster.Colors.getR

    let triggerRequestRender _ = requestRender.Trigger()

    let normalizeHistogramChanged _ =
        let should = control.normalizeHistogramCheckBox.IsChecked.GetValueOrDefault false
        if should
        then
            normalizeHandler <- data |> Observable.subscribe (fun r -> normalizeRenderer (getRectangle ()) r) |> Some
        else
            (Option.get normalizeHandler).Dispose()
            normalizeHandler <- None
        triggerRequestRender ()

    do
        let histogramChannel =
            control.histogramChannel.SelectionChanged
            |> Observable.map getHistogramChannel
        data
            |> Observable.map (fun r -> r.StreamPixels(getRectangle ()))
            |> Observable.combineLatest (fun a b -> (a, b)) histogramChannel
            |> Observable.map (fun (chan, p) -> generateHistogram (getBgColor ()) chan p)
            |> Observable.add (fun s -> histogramControl.UpdateHistogram s 0xffff0000)

        control.histogramChannel.SelectedIndex <- 0

        control.normalizeHistogramCheckBox.Checked.Add normalizeHistogramChanged
        control.normalizeHistogramCheckBox.Unchecked.Add normalizeHistogramChanged

    member this.RequestRender = requestRender.Publish