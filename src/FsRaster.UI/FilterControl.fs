namespace FsRaster.UI

open System

open FsXaml

open FsRaster.Utils
open FsRaster.CoreRendering
open FsRaster.Filters

type FilterControl = XAML<"FilterControl.xaml", true>

type FilterControlController(control : FilterControl, rectangle : SceneRectangleController, render : IEvent<IRenderer>, bgColorPicker : FsRaster.UI.ColorPicker.AdvancedColorPicker) =

    let requestRender = Event<unit>()

    let histogramControl = control.histogramControl :?> HistogramControl

    let getRectangle () =
        Option.map (fun (left, top, right, bottom) -> (left + 1, top + 1, right - 1, bottom - 1)) rectangle.Rectangle

    let getBgColor () = bgColorPicker.SelectedColor |> FsRaster.Colors.fromUIColor

    let getHistogramChannel _ =
        match control.histogramChannel.SelectedIndex with
        | 1 -> FsRaster.Colors.getG
        | 2 -> FsRaster.Colors.getB
        | _ -> FsRaster.Colors.getR

    let getHistogramColor _ =
        match control.histogramChannel.SelectedIndex with
        | 1 -> 0xff00ff00
        | 2 -> 0xff0000ff
        | _ -> 0xffff0000

    let triggerRequestRender _ = requestRender.Trigger()

    let normalizeHistogramOnRender ctx =
        if control.normalizeHistogramCheckBox.IsChecked.GetValueOrDefault(false)
        then
            Option.bind (fun rect ->
                normalizeHistogram ctx rect
                None
            ) (getRectangle ()) |> ignore

    let updateHistogram ctx =
        Option.bind (fun rect ->
            let histogram = generateHistogram (getBgColor ()) (getHistogramChannel ()) ctx rect
            histogramControl.UpdateHistogram histogram (getHistogramColor ())
            None
        ) (getRectangle ()) |> ignore

    let onRender (renderer : IRenderer) =
        normalizeHistogramOnRender renderer.Context
        updateHistogram renderer.Context

    do
        render.Add onRender

        control.normalizeHistogramCheckBox.Checked.Add triggerRequestRender
        control.normalizeHistogramCheckBox.Unchecked.Add triggerRequestRender
        control.histogramChannel.SelectionChanged.Add triggerRequestRender

    member this.RequestRender = requestRender.Publish