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

    let mutable duringBuild = false

    let histogramControl = control.histogramControl :?> HistogramControl
    let redFunctionDefinition = control.redFunctionDefinition :?> FunctionDef
    let greenFunctionDefinition = control.greenFunctionDefinition :?> FunctionDef
    let blueFunctionDefinition = control.blueFunctionDefinition :?> FunctionDef

    let generateGaussianMatrix size sigma' =
        let k = size / 2
        let sigma = sigma' * sigma' * 2.0
        let piSigma = sigma * Math.PI
        [ 0 .. size - 1 ] |> List.collect (fun y ->
            [ 0 .. size - 1] |> List.map (fun x ->
                let a = double <| (x - k - 1) * (x - k - 1)
                let b = double <| (y - k - 1) * (y - k - 1)
                (exp (-(a + b) / sigma)) / piSigma
            )
        )

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

    let getConvolutionControls () =
        let size = getConvolutionSize ()
        [ 0 .. size * size - 1 ]
        |> List.map (fun idx -> control.convolutionWeights.Children.Item idx :?> DoubleUpDown)

    let getConvolutionWeights () =
        getConvolutionControls () |> List.map (fun c -> double <| c.Value.GetValueOrDefault(0.0))

    let triggerRequestRender _ = if not duringBuild then requestRender.Trigger()

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

    let convolveImage ctx =
        Option.bind (fun rect ->
            let size = getConvolutionSize ()
            if size > 0 then
                let weights = getConvolutionWeights () |> List.toArray
                let coeff = control.convolutionCoeff.Value.GetValueOrDefault(1.0)
                let offset = control.convolutionOffset.Value.GetValueOrDefault(0.0)
                convolve ctx size weights offset coeff rect
            None
        ) (getRectangle ()) |> ignore

    let updateCoeffWeight _ =
        if not duringBuild && not (control.manualCoeff.IsChecked.GetValueOrDefault false)
        then
            let weights = getConvolutionWeights ()
            let sum = weights |> List.sum
            let sum' = if abs sum < 0.0001 then 1.0 else sum
            control.convolutionCoeff.Value <- Nullable(sum')

    let onRender (renderer : IRenderer) =
        normalizeHistogramOnRender renderer.Context
        convolveImage renderer.Context
        updateHistogram renderer.Context

    let prepareWeightControl idx =
        let ctrl = new DoubleUpDown()
        ctrl.Value <- Nullable(0.0)
        ctrl.ShowButtonSpinner <- false
        ctrl.ValueChanged.Add (updateCoeffWeight >> triggerRequestRender)
        ctrl.Width <- 30.0
        ctrl

    let onConvolutionSizeChanged _ =
        let size = getConvolutionSize ()
        control.convolutionFilter.IsEnabled <- size > 0
        control.loadGauss.IsEnabled <- size > 0
        if size <> 0
        then
            duringBuild <- true
            control.convolutionWeights.Children.Clear()
            let controls = [ 1 .. size * size ] |> List.map prepareWeightControl
            controls.[controls.Length / 2].Value <- Nullable(1.0)
            controls |> List.iter (control.convolutionWeights.Children.Add >> ignore)
            duringBuild <- false
            updateCoeffWeight ()

    let onLoadGaussMatrix _ =
        duringBuild <- true
        let size = getConvolutionSize ()
        let gauss = generateGaussianMatrix size 3.0
        let ctrls = getConvolutionControls ()
        ctrls |> List.zip gauss |> List.iter (fun (g, c) -> c.Value <- Nullable(g))
        duringBuild <- false
        updateCoeffWeight ()

    let clearFunctionDefs _ =
        redFunctionDefinition.ResetPoints()
        greenFunctionDefinition.ResetPoints()
        blueFunctionDefinition.ResetPoints()

    do
        render.Add onRender

        control.normalizeHistogramCheckBox.Checked.Add triggerRequestRender
        control.normalizeHistogramCheckBox.Unchecked.Add triggerRequestRender
        control.histogramChannel.SelectionChanged.Add triggerRequestRender

        control.loadGauss.Click.Add onLoadGaussMatrix

        control.convolutionFilterSize.SelectionChanged.Add onConvolutionSizeChanged
        control.convolutionCoeff.ValueChanged.Add triggerRequestRender
        control.convolutionOffset.ValueChanged.Add triggerRequestRender

        control.convolutionNone.Tag <- 0
        control.convolutionSize3.Tag <- 3
        control.convolutionSize5.Tag <- 5
        control.convolutionSize7.Tag <- 7

        control.convolutionFilterSize.SelectedIndex <- 1
        control.convolutionFilterSize.SelectedIndex <- 0

        control.functionFilterReset.Click.Add clearFunctionDefs

    member this.RequestRender = requestRender.Publish