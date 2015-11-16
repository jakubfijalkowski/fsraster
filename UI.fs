namespace FsRaster.UI

open System
open System.Collections
open System.Collections.ObjectModel

open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging

open FsXaml

open FsRaster.Utils
open FsRaster.Figures
open FsRaster.FigureRendering
open FsRaster.FigureBuilding
open FsRaster.FigureHitTests
open FsRaster.FigureClipping
open FsRaster.CoreRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type FigureNameConverter() =
    interface IValueConverter with
        member this.Convert(value, _, _, _) = longDescriptionOf (value :?> Figure) :> obj
        member this.ConvertBack(_, _, _, _) = failwith "Operation not supported."

type MoveDataType = ClipRectMove | ClipRectResize | FigureMove

type MainWindowController() =

    [<Literal>]
    let MatchDistance = 10

    let window = MainWindow()

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)

    let figures : Generic.IList<Figure> = ObservableCollection() :> Generic.IList<Figure>
    
    let mutable figureBuilder : FigureBuilder option = None

    let mutable moveData : (MoveDataType * Point * int) option = None

    let mutable clipRect : Rectangle option = None

    let getPosition (e : Input.MouseEventArgs) =
        let pos = e.GetPosition(window.imageContainer)
        (int pos.X, int pos.Y)

    let withMoveData t f =
        match moveData with
        | Some (dt, s, i) when dt = t ->
            match f s i with
            | Some n ->
                moveData <- Some (dt, n, i)
                true
            | None ->
                moveData <- None
                false
        | _ -> false

    let selectHitFigure pt =
        figures
            |> Seq.mapi (fun i f -> (i, isFigureHit pt f))
            |> Seq.filter (snd >> Option.isSome)
            |> Seq.append (seq { yield (-1, Some Int32.MaxValue) })
            |> Seq.minBy snd
            |> fst

    let getBuilderPreview color =
        let pos = Input.Mouse.GetPosition(window.imageContainer)
        let pt = (int pos.X, int pos.Y)
        Option.bind (previewFigure pt color) figureBuilder

    let getBuildInfo _ =
        { Color = window.figureColor.SelectedColor.Value; Thickness = window.figureThickness.Value.Value }

    let render' _ =
        let bgColor = window.backgroundColor.SelectedColor.Value
        let gridColor = window.gridColor.SelectedColor.Value
        let figColor = window.figureColor.SelectedColor.Value

        use context = new BitmapRenderer(mainCanvas.GetBitmapContext ReadWriteMode.ReadWrite) :> IRenderer 
        context.Clear bgColor

        if window.gridCheckBox.IsChecked.Value
        then renderGrid context window.gridSpacing.Value.Value gridColor

        let buildInfo = getBuildInfo ()
        let topMost =
            [ getBuilderPreview buildInfo
            ; Option.map (asPolygon Colors.Red) clipRect ]

        let figs = Option.fold clipFigures (figures :> Figure seq) clipRect

        renderFigures context (Seq.append figs (Seq.choose id topMost))

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

    let changeImageSize (e : SizeChangedEventArgs) =
        mainCanvas <- BitmapFactory.New(int e.NewSize.Width, int e.NewSize.Height)
        window.mainImage.Source <- mainCanvas
        render ()

    let updateSelectedFigure (e : SelectionChangedEventArgs) =
        let idx = window.figureList.SelectedIndex
        window.deleteMenu.IsEnabled <- idx > -1
        if idx > -1 then
            let info = getFigureInfo figures.[idx]
            window.figureColor.SelectedColor <- Nullable info.Color
            window.figureThickness.Value <- Nullable info.Thickness

    let deleteFigure _ =
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            figures.RemoveAt idx
            render ()

    let updateFigureInfo e =
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            figures.[idx] <- updateFigure (getBuildInfo ()) figures.[idx]
            window.figureList.SelectedIndex <- idx
            render ()

    let startBuildingFigure (e : RoutedEventArgs) =
        let figure = (e.Source :?> MenuItem).Tag :?> Figure
        figureBuilder <- Some (getFigureBuilder figure)

    let trySelectFigure (e : Input.MouseEventArgs) =
        let pos = getPosition e
        let idx = selectHitFigure pos
        window.figureList.SelectedIndex <- idx
        moveData <- if idx > -1 then Some (FigureMove, pos, idx) else None
        idx > -1

    let tryMoveFigure (e : Input.MouseEventArgs) =
        withMoveData FigureMove (fun s idx ->
            let pos = getPosition e
            figures.[idx] <- moveFigure (pos -~ s) figures.[idx]
            Some pos
        )

    let tryProcessFigure (e : Input.MouseEventArgs) =
        match figureBuilder with
        | Some b ->
            let info = getBuildInfo ()
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
            let info = getBuildInfo ()
            Option.map (figures.Add) (forceFinishFigure b info) |> ignore
            figureBuilder <- None
            render ()
        | _ -> ()

    let enableClipping _ =
        let w = window.imageContainer.ActualWidth
        let h = window.imageContainer.ActualHeight
        let left = int (w * 0.2)
        let right = int (w * 0.8)
        let top = int (h * 0.2)
        let bottom = int (h * 0.8)
        clipRect <- Some (left, top, right, bottom)
        render ()

    let tryStartMovingClipping (e : Input.MouseEventArgs) =
        match clipRect with
        | Some (left, top, _, _) ->
            let pos = getPosition e
            if distance pos (left, top) <= MatchDistance
            then
                moveData <- Some (ClipRectMove, pos, 0)
                true
            else false
        | None -> false

    let tryMoveClipping (e : Input.MouseEventArgs) =
        withMoveData ClipRectMove (fun s _ ->
            let pos = getPosition e
            clipRect <- Option.map (moveRect (pos -~ s)) clipRect
            Some pos
        )

    let tryStartResizingClipping (e : Input.MouseEventArgs) =
        match clipRect with
        | Some (_, _, right, bottom) ->
            let pos = getPosition e
            if distance pos (right, bottom) <= MatchDistance
            then
                moveData <- Some (ClipRectResize, pos, 0)
                true
            else false
        | None -> false

    let tryResizeClipping (e : Input.MouseEventArgs) =
        withMoveData ClipRectResize (fun s _ ->
            let pos = getPosition e
            clipRect <- Option.map (resizeRectMin (MatchDistance * 2) (pos -~ s)) clipRect
            Some pos
        )

    let disableClipping _ =
        clipRect <- None
        render ()

    let updateMouseCursor (e : Input.MouseEventArgs) =
        let pos = getPosition e
        let result =
            Option.bind (fun (left, top, right, bottom) ->
                if distance pos (left, top) <= MatchDistance
                then Some Input.Cursors.SizeAll
                else if distance pos (right, bottom) <= MatchDistance
                then Some Input.Cursors.SizeNWSE
                else None
            ) clipRect
        window.Root.Cursor <- Option.fold (fun _ c -> c) Input.Cursors.Arrow result

    let onImageMouseDown e =
        Input.Mouse.Capture window.imageContainer |> ignore
        if Option.isNone figureBuilder
        then (tryStartResizingClipping e || tryStartMovingClipping e || trySelectFigure e) |> ignore

    let onImageMouseMove e =
        if tryResizeClipping e || tryMoveClipping e || tryMoveFigure e || Option.isSome figureBuilder
        then render ()
        else updateMouseCursor e

    let onImageMouseUp e =
        if tryProcessFigure e then render ()
        moveData <- None
        Input.Mouse.Capture null |> ignore

    let onKeyUp (e : Input.KeyEventArgs) =
        if e.Key = Input.Key.F then finishCurrentFigure ()

    let addRandomFigures _ =
        let rnd = System.Random(0xB15B00B5)
        let genFig t p1 p2 c =
            match t with
            | 0 -> Point (p1, c)
            | 1 -> Line (p1, p2, c)
            | 2 -> Circle (p1, distance p1 p2, c)
            | _ -> AntialiasedCircle (p1, distance p1 p2, c)
        [1 .. 200]
            |> List.map (fun _ ->
                let t = rnd.Next(4)
                let p1 = (rnd.Next(int mainCanvas.Width), rnd.Next(int mainCanvas.Height))
                let p2 = (rnd.Next(int mainCanvas.Width), rnd.Next(int mainCanvas.Height))
                let c = UIColors.allColors.[rnd.Next(UIColors.allColors.Length)]
                genFig t p1 p2 c.Color
                )
            |> List.iter figures.Add
        render ()

    do
        window.backgroundColor.SelectedColor <- Nullable Colors.White
        window.figureColor.SelectedColor <- Nullable Colors.Black
        window.gridColor.SelectedColor <- Nullable Colors.Black

        window.figureList.ItemsSource <- figures

        availableFigures
        |> List.map (fun f ->
            let mi = MenuItem()
            mi.Header <- "_" + shortDescriptionOf f
            mi.Click.Add startBuildingFigure
            mi.Tag <- f
            mi)
        |> List.iter (window.addMenu.Items.Add >> ignore)

        window.imageContainer.SizeChanged.Add changeImageSize
        window.imageContainer.MouseDown.Add onImageMouseDown
        window.imageContainer.MouseMove.Add onImageMouseMove
        window.imageContainer.MouseUp.Add onImageMouseUp

        window.figureList.SelectionChanged.Add updateSelectedFigure
        window.figureColor.SelectedColorChanged.Add updateFigureInfo
        window.figureThickness.ValueChanged.Add updateFigureInfo
        window.gridCheckBox.Checked.Add render
        window.gridCheckBox.Unchecked.Add render
        window.gridSpacing.ValueChanged.Add render
        window.gridColor.SelectedColorChanged.Add render
        window.backgroundColor.SelectedColorChanged.Add render

        window.deleteMenu.Click.Add deleteFigure
        window.addRandomMenu.Click.Add addRandomFigures

        window.Root.KeyUp.Add onKeyUp
        window.clipCheckBox.Checked.Add enableClipping
        window.clipCheckBox.Unchecked.Add disableClipping

    member this.Window with get() = window.Root
