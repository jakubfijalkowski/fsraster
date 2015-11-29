namespace FsRaster.UI

open System
open System.Windows.Media

open Microsoft.Win32

open FsXaml

open FsRaster
open FsRaster.Utils
open FsRaster.Figures

type FigureInfoPicker = XAML<"FigureInfoPicker.xaml", true>

type FigureInfoPickerController(control : FigureInfoPicker) =

    let infoChanged = new Event<FigureInfo>()

    let mutable duringUpdate = false

    let mutable currentInfo = { Color = FigureColor.Color 0; Thickness = 1; Filled = None }
    let mutable isFigureSelected = false

    let triggerChanged () =
        if not duringUpdate then infoChanged.Trigger currentInfo

    let currentColor _ =
        FigureColor.makeColor (control.figureColor.SelectedColor.GetValueOrDefault Colors.White)

    let updateColor _ =
        currentInfo <- { currentInfo with Color = currentColor () }
        triggerChanged ()

    let updateThickness _ =
        let thickness = control.figureThickness.Value.GetValueOrDefault 0
        currentInfo <- { currentInfo with Thickness = thickness }
        triggerChanged ()

    let selectNewTexture path =
        let tex = FigureColor.fromImage path
        currentInfo <- { currentInfo with Color = tex }
        triggerChanged ()

    let rollBackToColor _ =
        control.useFigureColor.IsChecked <- Nullable true

    let askForTexture _ =
        let picker = new OpenFileDialog()
        picker.CheckFileExists <- true
        picker.Filter <- "Images (*.bmp, *.jpg, *.png)|*.bmp;*.jpg;*.png"
        if picker.ShowDialog().GetValueOrDefault false
        then
            selectNewTexture picker.FileName
            true
        else
            false

    let updateFigureFillType _ =
        let value = control.isFigureFilled.IsChecked.GetValueOrDefault false
        currentInfo <- { currentInfo with Filled = Some value }
        triggerChanged ()

    let enableColor _ =
        control.useFigureColor.IsChecked <- Nullable true
        control.figureColor.IsEnabled <- true
        control.selectTextureButton.IsEnabled <- false

        control.figureColor.SelectedColor <- Nullable <| Colors.toUIColor (FigureColor.getColor currentInfo.Color)
        
        control.isFigureFilled.IsEnabled <- isFigureSelected && Option.isSome currentInfo.Filled
        control.isFigureFilled.IsChecked <- Nullable <| (isFigureSelected && Option.opt false currentInfo.Filled)

        control.isTextureReduced.IsEnabled <- false
        control.isTextureReduced.IsChecked <- Nullable false
        control.reductionValue.IsEnabled <- false

    let enableTexture _ =
        control.useFigureTexture.IsChecked <- Nullable true
        control.figureColor.IsEnabled <- false
        control.selectTextureButton.IsEnabled <- true

        // Should be true already
        control.isFigureFilled.IsEnabled <- true
        control.isFigureFilled.IsChecked <- Nullable true

        control.isTextureReduced.IsEnabled <- true
        control.isTextureReduced.IsChecked <- Nullable false
        control.reductionValue.IsEnabled <- FigureColor.isReducedTexture currentInfo.Color

    let resetControlsState _ =
        currentInfo <- { Color = FigureColor.makeColor Colors.Black; Thickness = 1; Filled = None }
        control.figureThickness.Value <- Nullable 1
        control.reductionValue.Value <- Nullable (pown 2 24)
        control.useFigureTexture.IsEnabled <- isFigureSelected
        enableColor ()

    let decideColorTypeForFigure info =
        let mayHaveTexture = Option.opt false info.Filled
        let hasTexture = FigureColor.isTexture info.Color
        let reallyHasTexture = mayHaveTexture && hasTexture

        control.useFigureTexture.IsEnabled <- mayHaveTexture
        control.selectTextureButton.IsEnabled <- reallyHasTexture
        control.useFigureColor.IsChecked <- Nullable (not reallyHasTexture)
        control.useFigureTexture.IsChecked <- Nullable reallyHasTexture
        control.figureColor.IsEnabled <- not reallyHasTexture

        control.isFigureFilled.IsEnabled <- Option.isSome info.Filled
        control.isFigureFilled.IsChecked <- Nullable (Option.opt false info.Filled)
        
        control.isTextureReduced.IsEnabled <- reallyHasTexture
        control.isTextureReduced.IsChecked <- Nullable (FigureColor.isReducedTexture info.Color)
        control.reductionValue.IsEnabled <- reallyHasTexture && FigureColor.isReducedTexture info.Color

        if FigureColor.isReducedTexture info.Color then
            let tx = FigureColor.asReducedTexture info.Color
            control.reductionValue.Value <- Nullable tx.Colors

        if not reallyHasTexture then
            control.figureColor.SelectedColor <- Nullable <| Colors.toUIColor (FigureColor.getColor info.Color)

    let onTypeChanged _ =
        if not duringUpdate then
            if control.useFigureColor.IsChecked.GetValueOrDefault true
            then
                enableColor ()
                updateColor ()
            else
                if askForTexture () then enableTexture () else enableColor ()

    let onReduceChanged _ =
        let enabled = control.isTextureReduced.IsChecked.GetValueOrDefault false
        let value = control.reductionValue.Value.GetValueOrDefault (pown 2 24)
        control.reductionValue.IsEnabled <- enabled
        if enabled
        then
            currentInfo <- { currentInfo with Color = FigureColor.reduceTexture value currentInfo.Color }
        else
            currentInfo <- { currentInfo with Color = FigureColor.revertReduceTexture currentInfo.Color }
        triggerChanged ()

    let onChangedReductionValue _ =
        if FigureColor.isReducedTexture currentInfo.Color && not duringUpdate then
            let value = control.reductionValue.Value.GetValueOrDefault (pown 2 24)
            currentInfo <- { currentInfo with Color = FigureColor.reduceTexture value currentInfo.Color }
            triggerChanged ()

    do
        resetControlsState ()
        control.figureColor.SelectedColor <- Nullable Colors.Black

        control.useFigureColor.Checked.Add onTypeChanged
        control.useFigureTexture.Checked.Add onTypeChanged

        control.figureColor.SelectedColorChanged.Add updateColor
        control.selectTextureButton.Click.Add (askForTexture >> ignore)

        control.figureThickness.ValueChanged.Add updateThickness

        control.isFigureFilled.Checked.Add updateFigureFillType
        control.isFigureFilled.Unchecked.Add updateFigureFillType

        control.isTextureReduced.Checked.Add onReduceChanged
        control.isTextureReduced.Unchecked.Add onReduceChanged
        control.reductionValue.ValueChanged.Add onChangedReductionValue

    member x.UpdateSelectedFigure figOpt =
        duringUpdate <- true

        match figOpt with
        | Some fig ->
            let info = getFigureInfo fig
            isFigureSelected <- true
            currentInfo <- info

            decideColorTypeForFigure info
            control.figureThickness.Value <- Nullable info.Thickness
        | None ->
            isFigureSelected <- false
            resetControlsState ()

        duringUpdate <- false

    member x.FigureInfo = currentInfo

    member x.FigureInfoChanged = infoChanged.Publish
