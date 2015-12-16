namespace FsRaster.UI

open System
open System.Collections
open System.Collections.ObjectModel

open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging

open Microsoft.Win32

open FsXaml

open FsRaster
open FsRaster.Utils
open FsRaster.UI
open FsRaster.UI.Utils
open FsRaster.Figures
open FsRaster.FigureRendering
open FsRaster.FigureBuilding
open FsRaster.FigureHitTests
open FsRaster.FigureClipping
open FsRaster.Resources
open FsRaster.CoreRendering
open FsRaster.CoreAlgorithms

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let window = MainWindow()
    let figureInfoPicker = FigureInfoPickerController(window.figureInfoPicker :?> FigureInfoPicker)
    let clippingRectangle = SceneRectangleController(window.overlayMouse)
    let filteringRectangle = SceneRectangleController(window.overlayMouse)

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)

    let figures : Generic.IList<Figure> = ObservableCollection() :> Generic.IList<Figure>
    
    let mutable figureBuilder : FigureBuilder option = None
    let mutable moveData : (Point * int) option = None
    let mutable fillAction : (int -> int -> IRenderer -> unit) option = None

    let mutable duringFigureUpdate = false

    let renderEvent = Event<IRenderer>()

    // HELPERS

    let getPosition (e : Input.MouseEventArgs) =
        let pos = e.GetPosition(window.imageContainer)
        (int pos.X, int pos.Y)

    let withMoveData f =
        match moveData with
        | Some (s, i) ->
            match f s i with
            | Some n ->
                moveData <- Some (n, i)
                true
            | None ->
                moveData <- None
                false
        | _ -> false

    let cancelAllActions _ =
        moveData <- None
        figureBuilder <- None
        fillAction <- None

    let selectHitFigure pt =
        figures
            |> Seq.mapi (fun i f -> (i, isFigureHit pt f))
            |> Seq.filter (snd >> Option.isSome)
            |> Seq.append (seq { yield (-1, Some Int32.MaxValue) })
            |> Seq.rev
            |> Seq.minBy snd
            |> fst

    let getBuilderPreview color =
        let pos = Input.Mouse.GetPosition(window.imageContainer)
        let pt = (int pos.X, int pos.Y)
        Option.bind (previewFigure pt color) figureBuilder

    // RENDERING

    let clearBackground (ctx : IRenderer) =
        let bgColor = window.backgroundColor.SelectedColor
        ctx.Clear bgColor

    let getGrid (ctx : IRenderer) =
        if window.gridCheckBox.IsChecked.Value
        then
            let spacing = window.gridSpacing.Value.GetValueOrDefault 10
            let gridColor = window.gridColor.SelectedColor
            generateGrid ctx.Width ctx.Height spacing gridColor
        else []

    let getTopMostFigures (ctx : IRenderer) =
        let clipRect = clippingRectangle.Rectangle
        let filterRect = filteringRectangle.Rectangle
        let buildInfo = figureInfoPicker.FigureInfo
        Seq.choose id
            [ getBuilderPreview buildInfo
            ; Option.map (asPolygon Colors.Red) clipRect
            ; Option.map (asPolygon Colors.Blue) filterRect ]

    let clipRenderFigures (ctx : IRenderer) figures =
        let clipRect = clippingRectangle.Rectangle
        Option.fold clipFigures (figures :> Figure seq) clipRect

    let processYUVPlane (ctx : IRenderer) =
        if window.yuvCheckbox.IsChecked.GetValueOrDefault(false) then
            let plane =
                match window.yuvPlane.SelectedIndex with
                | 1 -> PlaneU
                | 2 -> PlaneV
                | _ -> PlaneY
            ctx.ExtractYUVPlane plane

    let renderPipeline (ctx : IRenderer) =
        clearBackground ctx
        let grid = getGrid ctx :> Figure seq |> clipRenderFigures ctx
        let figs = figures :> Figure seq |> clipRenderFigures ctx
        let topMost = getTopMostFigures ctx
        renderFigures ctx (seq { yield! grid; yield! figs; yield! topMost })
        processYUVPlane ctx

    let render' _ =
        use context = new BitmapRenderer(mainCanvas.GetBitmapContext ReadWriteMode.ReadWrite) :> IRenderer
        renderPipeline context
        renderEvent.Trigger context

    let render _ =
        #if DEBUG || PROFILE_RENDERING
        let sw = Diagnostics.Stopwatch()
        sw.Start()
        render' ()
        sw.Stop()
        window.Root.Title <- "Render time: " + sw.ElapsedMilliseconds.ToString() + " ms"
        #else
        render' ()
        #endif

    // FIGURE UPDATES

    let deleteFigure _ =
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            figures.RemoveAt idx
            render ()

    let updateSelectedFigure (e : SelectionChangedEventArgs) =
        if not duringFigureUpdate && Option.isNone moveData then
            let idx = window.figureList.SelectedIndex
            window.deleteMenu.IsEnabled <- idx > -1
            if idx > -1
            then
                let fig = figures.[idx]
                figureInfoPicker.UpdateSelectedFigure (Some fig)
            else
                figureInfoPicker.UpdateSelectedFigure None

    let updateFigureInfo _ =
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            duringFigureUpdate <- true
            figures.[idx] <- updateFigure figureInfoPicker.FigureInfo figures.[idx]
            window.figureList.SelectedIndex <- idx
            render ()
            duringFigureUpdate <- false

    // FIGURE BUILDING/MOVING

    let startBuildingFigure (e : RoutedEventArgs) =
        cancelAllActions ()
        let figure = (e.Source :?> MenuItem).Tag :?> Figure
        figureBuilder <- Some (getFigureBuilder figure)

    let trySelectFigure (e : Input.MouseEventArgs) =
        let pos = getPosition e
        let idx = selectHitFigure pos
        window.figureList.SelectedIndex <- idx
        moveData <- if idx > -1 then Some (pos, idx) else None
        idx > -1

    let tryMoveFigure (e : Input.MouseEventArgs) =
        withMoveData (fun s idx ->
            let pos = getPosition e
            figures.[idx] <- moveFigure (pos -~ s) figures.[idx]
            window.figureList.SelectedIndex <- idx
            Some pos
        )

    let tryProcessFigure (e : Input.MouseEventArgs) =
        match figureBuilder with
        | Some b ->
            let info = figureInfoPicker.FigureInfo
            match processBuildingFigure b (getPosition e) info with
            | Choice1Of2 f ->
                figures.Add f
                figureBuilder <- None
            | Choice2Of2 b -> figureBuilder <- Some b
            true
        | _ -> false

    let finishCurrentFigure _ =
        match figureBuilder with
        | Some b ->
            let info = figureInfoPicker.FigureInfo
            Option.map (figures.Add) (forceFinishFigure b info) |> ignore
            figureBuilder <- None
            render ()
        | _ -> ()

    // CLIPPING/FILTERING

    let toggleClipping _ =
        clippingRectangle.IsEnabled <- window.clipCheckBox.IsChecked.GetValueOrDefault false
        render ()

    let toggleFiltering _ =
        filteringRectangle.IsEnabled <- window.filterCheckBox.IsChecked.GetValueOrDefault false
        render ()

    // FILLING

    let tryFillingRenderer (e : Input.MouseEventArgs) =
        match fillAction with
        | Some action ->
            let x, y = getPosition e
            use context = new BitmapRenderer(mainCanvas.GetBitmapContext ReadWriteMode.ReadWrite) :> IRenderer 
            action x y context
            fillAction <- None
            true
        | None -> false

    let startFilling4 _ =
        cancelAllActions ()
        let bColor = window.boundaryColor.SelectedColor
        let fColor = window.fillColor.SelectedColor
        fillAction <- Some (boundaryFill4 bColor fColor)

    let startFilling8 _ =
        cancelAllActions ()
        let bColor = window.boundaryColor.SelectedColor
        let fColor = window.fillColor.SelectedColor
        fillAction <- Some (boundaryFill8 bColor fColor)

    // MOUSE CURSOR

    let updateMouseCursor (e : Input.MouseEventArgs option) =
        let pos = Option.withOpt getPosition (-999, -999) e
        let r1 = Option.map (fun _ -> loadCursorFile "bucket_cursor") fillAction
        let r2 = Option.map (fun _ -> Input.Cursors.Hand) figureBuilder
        let r3 = Option.map (fun _ -> Input.Cursors.SizeAll) moveData
        window.imageContainer.Cursor <- List.choose id [r1; r2; r3; Some Input.Cursors.Arrow] |> List.head

    // EVENTS

    let onImageMouseDown e =
        if not (tryFillingRenderer e) then
            Input.Mouse.Capture window.imageContainer |> ignore
            if Option.isNone figureBuilder
            then (trySelectFigure e) |> ignore
            updateMouseCursor (Some e)

    let onImageMouseMove e =
        updateMouseCursor (Some e)
        if tryMoveFigure e || Option.isSome figureBuilder
        then render ()

    let onImageMouseUp e =
        if tryProcessFigure e then render ()
        moveData <- None
        Input.Mouse.Capture null |> ignore
        updateMouseCursor (Some e)

    let onKeyUp (e : Input.KeyEventArgs) =
        match e.Key with
        | Input.Key.F -> finishCurrentFigure ()
        | Input.Key.Escape ->
            cancelAllActions ()
            updateMouseCursor None
            Input.Mouse.Capture null |> ignore
        | _ -> ()

    let onSizeChanged (e : SizeChangedEventArgs) =
        mainCanvas <- BitmapFactory.New(int e.NewSize.Width, int e.NewSize.Height)
        window.mainImage.Source <- mainCanvas
        render ()

    do
        window.backgroundColor.SelectedColor <- Colors.White
        window.gridColor.SelectedColor <- Colors.Black
        window.boundaryColor.SelectedColor <- Colors.Red
        window.fillColor.SelectedColor <- Colors.Green

        window.figureList.ItemsSource <- figures

        availableFigures
        |> List.map (fun f ->
            let mi = MenuItem()
            mi.Header <- "_" + UIUtils.shortDescriptionOf f
            mi.Click.Add startBuildingFigure
            mi.Tag <- f
            mi)
        |> List.iter (window.addMenu.Items.Add >> ignore)

        window.imageContainer.SizeChanged.Add onSizeChanged
        window.imageContainer.MouseDown.Add onImageMouseDown
        window.imageContainer.MouseMove.Add onImageMouseMove
        window.imageContainer.MouseUp.Add onImageMouseUp

        window.figureList.SelectionChanged.Add updateSelectedFigure
        figureInfoPicker.FigureInfoChanged.Add updateFigureInfo

        window.gridCheckBox.Checked.Add render
        window.gridCheckBox.Unchecked.Add render
        window.gridSpacing.ValueChanged.Add render
        window.gridColor.SelectedColorChanged.Add render
        window.backgroundColor.SelectedColorChanged.Add render

        window.deleteMenu.Click.Add deleteFigure

        window.Root.KeyUp.Add onKeyUp

        window.fill4.Click.Add startFilling4
        window.fill8.Click.Add startFilling8

        window.yuvCheckbox.Checked.Add render
        window.yuvCheckbox.Unchecked.Add render

        window.yuvPlane.SelectionChanged.Add render

        window.clipCheckBox.Checked.Add toggleClipping
        window.clipCheckBox.Unchecked.Add toggleClipping
        clippingRectangle.IsEnabled <- false
        clippingRectangle.RequestRender.Add render

        window.filterCheckBox.Checked.Add toggleFiltering
        window.filterCheckBox.Unchecked.Add toggleFiltering
        filteringRectangle.IsEnabled <- false
        filteringRectangle.RequestRender.Add render

    member this.Window with get() = window.Root
