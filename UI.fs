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
open FsRaster.CoreRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type FigureNameConverter() =
    interface IValueConverter with
        member this.Convert(value, _, _, _) = longDescriptionOf (value :?> Figure) :> obj
        member this.ConvertBack(_, _, _, _) = failwith "Operation not supported."

type MainWindowController() =
    let window = MainWindow()

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)

    let figures : Generic.IList<Figure> = ObservableCollection() :> Generic.IList<Figure>
    
    let mutable figureBuilder : FigureBuilder option = None

    let mutable moveData : (Point * int) option = None

    let getPosition (e : Input.MouseEventArgs) =
        let pos = e.GetPosition(window.imageContainer)
        (int pos.X, int pos.Y)

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
        match figureBuilder with
        | Some b -> previewFigure b pt color
        | None   -> Seq.empty

    let render _ =
        let bgColor = window.backgroundColor.SelectedValue :?> Color
        let gridColor = window.gridColor.SelectedValue :?> Color
        let figColor = window.figureColor.SelectedValue :?> Color

        use context = new BitmapRenderer(mainCanvas.GetBitmapContext ReadWriteMode.ReadWrite) :> IRenderer 
        context.Clear bgColor

        if window.gridCheckBox.IsChecked.Value
        then renderGrid context window.gridSpacing.Value.Value gridColor

        renderFigures context (Seq.append figures (getBuilderPreview figColor))

    let changeImageSize (e : SizeChangedEventArgs) =
        mainCanvas <- BitmapFactory.New(int e.NewSize.Width, int e.NewSize.Height)
        window.mainImage.Source <- mainCanvas
        render ()

    let updateSelectedFigure (e : SelectionChangedEventArgs) =
        let idx = window.figureList.SelectedIndex
        window.deleteMenu.IsEnabled <- idx > -1
        if idx > -1 then window.figureColor.SelectedValue <- getFigureColor figures.[idx]

    let deleteFigure _ =
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            figures.RemoveAt idx
            render ()

    let updateFigureColor (e : SelectionChangedEventArgs) =
        let color = (e.AddedItems.[0] :?> UIColors.UIColor).Color
        let idx = window.figureList.SelectedIndex
        if idx > -1 then
            figures.[idx] <- updateFigureColor color figures.[idx]
            window.figureList.SelectedIndex <- idx
            render ()

    let startBuildingFigure (e : RoutedEventArgs) =
        let figure = (e.Source :?> MenuItem).Tag :?> Figure
        figureBuilder <- Some (getFigureBuilder figure)

    let selectFigure (e : Input.MouseEventArgs) =
        let pos = getPosition e
        let idx = selectHitFigure pos
        window.figureList.SelectedIndex <- idx
        moveData <- if idx > -1 then Some (pos, idx) else None

    let tryMoveFigure (e : Input.MouseEventArgs) =
        match moveData with
        | Some (s, idx) ->
            let pos = getPosition e
            figures.[idx] <- moveFigure (pos -~ s) figures.[idx]
            moveData <- Some (pos, idx)
            true
        | None -> false

    let tryProcessFigure (e : Input.MouseEventArgs) =
        match figureBuilder with
        | Some b ->
            let color = window.figureColor.SelectedValue :?> Color
            match processBuildingFigure b (getPosition e) color with
            | Choice1Of2 f ->
                figures.Add f
                figureBuilder <- None
            | Choice2Of2 b -> figureBuilder <- Some b
            true
        | _ -> false

    let onImageMouseDown e =
        Input.Mouse.Capture window.imageContainer |> ignore
        selectFigure e

    let onImageMouseMove e =
        if tryMoveFigure e || Option.isSome figureBuilder then render ()

    let onImageMouseUp e =
        if tryProcessFigure e then render ()
        let idx = window.figureList.SelectedIndex
        moveData <- None
        Input.Mouse.Capture null |> ignore
        printf "%d" idx

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
        Control.prepareColorBox window.backgroundColor
        Control.prepareColorBox window.figureColor
        Control.prepareColorBox window.gridColor
        window.backgroundColor.SelectedValue <- Colors.White

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
        window.figureColor.SelectionChanged.Add updateFigureColor
        window.gridCheckBox.Checked.Add render
        window.gridCheckBox.Unchecked.Add render
        window.gridSpacing.ValueChanged.Add render
        window.gridColor.SelectionChanged.Add render
        window.backgroundColor.SelectionChanged.Add render

        window.deleteMenu.Click.Add deleteFigure
        window.addRandomMenu.Click.Add addRandomFigures

    member this.Window with get() = window.Root