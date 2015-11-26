namespace FsRaster.UI

open System
open System.Windows.Media

open Microsoft.Win32

open FsXaml

open FsRaster
open FsRaster.Utils
open FsRaster.Figures

type FigureInfoPicker = XAML<"UI.FigureInfoPicker.xaml", true>

type FigureInfoPickerController(control : FigureInfoPicker) =

    let infoChanged = new Event<FigureInfo>()

    let mutable duringUpdate = false

    let mutable currentInfo = { Color = FigureColor.fromColor Colors.Black; Thickness = 1; Filled = None }

    let triggerChanged () =
        if not duringUpdate then infoChanged.Trigger currentInfo

    let currentColor _ =
        FigureColor.fromColor <| control.figureColor.SelectedColor.GetValueOrDefault Colors.White

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

    let askForTexture shouldRollBack _ =
        let picker = new OpenFileDialog()
        picker.CheckFileExists <- true
        picker.Filter <- "Images (*.bmp, *.jpg, *.png)|*.bmp;*.jpg;*.png"
        if picker.ShowDialog().GetValueOrDefault false
        then
            selectNewTexture picker.FileName
        else
            if shouldRollBack then rollBackToColor ()

    let updateFigureFillType _ =
        let value = control.isFigureFilled.IsChecked.GetValueOrDefault false
        currentInfo <- { currentInfo with Filled = Some value }
        triggerChanged ()

    let decideColorTypeForFigure info =
        let mayHaveTexture = Option.opt false info.Filled
        let hasTexture = FigureColor.isTexture info.Color
        let reallyHasTexture = mayHaveTexture && hasTexture

        control.useFigureTexture.IsEnabled <- mayHaveTexture
        control.selectTextureButton.IsEnabled <- mayHaveTexture
        control.useFigureColor.IsChecked <- Nullable (not reallyHasTexture)
        control.useFigureTexture.IsChecked <- Nullable reallyHasTexture

        if not reallyHasTexture then
            control.figureColor.SelectedColor <- Nullable (FigureColor.getColor info.Color)

    let onTypeChanged _ =
        if not duringUpdate then
            if control.useFigureColor.IsChecked.GetValueOrDefault true
            then
                control.figureColor.IsEnabled <- true
                control.selectTextureButton.IsEnabled <- false
                updateColor ()
            else
                control.figureColor.IsEnabled <- false
                control.selectTextureButton.IsEnabled <- true
                askForTexture true ()

    do
        control.figureColor.SelectedColor <- Nullable Colors.Black

        control.useFigureColor.Checked.Add onTypeChanged
        control.useFigureTexture.Checked.Add onTypeChanged

        control.figureColor.SelectedColorChanged.Add updateColor
        control.selectTextureButton.Click.Add (askForTexture false)

        control.figureThickness.ValueChanged.Add updateThickness

        control.isFigureFilled.Checked.Add updateFigureFillType
        control.isFigureFilled.Unchecked.Add updateFigureFillType

    member x.UpdateSelectedFigure figOpt =
        duringUpdate <- true

        match figOpt with
        | Some fig ->
            let info = getFigureInfo fig
            decideColorTypeForFigure info
            control.figureThickness.Value <- Nullable info.Thickness
            control.isFigureFilled.IsEnabled <- Option.isSome info.Filled
            control.isFigureFilled.IsChecked <- Nullable (Option.opt false info.Filled)
        | None ->
            currentInfo <- { currentInfo with Color = currentColor () }
            control.useFigureTexture.IsEnabled <- true
            control.useFigureColor.IsChecked <- Nullable true
            control.isFigureFilled.IsEnabled <- false

        duringUpdate <- false

    member x.FigureInfo = currentInfo

    member x.FigureInfoChanged = infoChanged.Publish
