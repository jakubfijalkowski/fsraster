namespace FsRaster.UI

open System
open System.Windows
open System.Windows.Controls

open Xceed.Wpf.Toolkit

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

    let getConvolutionSize () =
        let selectedItem = control.convolutionFilterSize.SelectedItem :?> ComboBoxItem
        selectedItem.Tag :?> int

    let getConvolutionWeights () =
        let size = getConvolutionSize ()
        [ 0 .. size * size - 1 ]
        |> List.map (fun idx -> (control.convolutionWeights.Children.Item idx :?> DoubleUpDown).Value.GetValueOrDefault 0.0)

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

    let updateCoeffWeight _ =
        if not (control.manualCoeff.IsChecked.GetValueOrDefault false)
        then
            let weights = getConvolutionWeights ()
            let sum = weights |> List.sum
            let sum' = if abs sum < 0.0001 then 1.0 else sum
            control.convolutionCoeff.Value <- Nullable(sum')

    let onRender (renderer : IRenderer) =
        normalizeHistogramOnRender renderer.Context
        updateHistogram renderer.Context

    let prepareWeightControl idx =
        let ctrl = new DoubleUpDown()
        ctrl.Value <- Nullable(0.0)
        ctrl.ShowButtonSpinner <- false
        ctrl.ValueChanged.Add updateCoeffWeight
        ctrl.Width <- 30.0
        ctrl

    let onConvolutionSizeChanged _ =
        let size = getConvolutionSize ()
        control.convolutionFilter.IsEnabled <- size > 0
        if size <> 0
        then
            control.convolutionWeights.Children.Clear()
            [ 1 .. size * size ]
            |> List.map prepareWeightControl
            |> List.iter (control.convolutionWeights.Children.Add >> ignore)
            updateCoeffWeight ()

    do
        render.Add onRender

        control.normalizeHistogramCheckBox.Checked.Add triggerRequestRender
        control.normalizeHistogramCheckBox.Unchecked.Add triggerRequestRender
        control.histogramChannel.SelectionChanged.Add triggerRequestRender

        control.convolutionFilterSize.SelectionChanged.Add onConvolutionSizeChanged
        control.convolutionNone.Tag <- 0
        control.convolutionSize3.Tag <- 3
        control.convolutionSize5.Tag <- 5
        control.convolutionSize7.Tag <- 7

        control.convolutionFilterSize.SelectedIndex <- 1
        control.convolutionFilterSize.SelectedIndex <- 0

    member this.RequestRender = requestRender.Publish