module FsRaster.UI

open System
open System.Collections
open System.Collections.ObjectModel

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging

open Xceed.Wpf.Toolkit

open FsRaster.Utils
open FsRaster.Figures
open FsRaster.CoreRendering

// This, probably, should not be let-functions, as they introduce side effects all the time
module Control =
    let positionIn (g : Grid) c x y =
        Grid.SetColumn(c, x)
        Grid.SetRow(c, y)
        g.Children.Add c |> ignore

    let prepare' (f : unit -> 'a when 'a :> FrameworkElement) m : 'a =
        let c = f ()
        c.Margin <- Thickness(m)
        c

    let prepare(f : unit -> 'a when 'a :> FrameworkElement) : 'a = prepare' f 5.0

    let addMenuItem (m : Menu) (mi : MenuItem) header handler =
        mi.Header <- header
        mi.Click.Add handler
        m.Items.Add mi |> ignore

    let newMenuItem handler header =
        let mi = MenuItem()
        mi.Header <- header
        mi.Click.Add handler
        mi

    let prepareColorBox _ =
        let cb = prepare ComboBox
        cb.DisplayMemberPath <- "Name"
        cb.SelectedValuePath <- "Color"
        cb.ItemsSource <- UIColors.allColors
        cb.SelectedValue <- Colors.Black
        cb


let whichFigureWasHit pt figures =
    let rec isFigHit i = function
        | ListCons (x, xs) -> if isFigureHit pt x then i else isFigHit (i + 1) xs
        | ListNil          -> -1
    isFigHit 0 (ConsList.ofList figures)

type FigureAction =
    | FigureModified of int
    | FigureAdded
    | FigureRemoved  of int

type MainWindow() =
    inherit Window()

    let mainImage = Image()
    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)
    let figureList : ListView = Control.prepare ListView
    let figureDescriptions : ObservableCollection<string> = ObservableCollection()
    let colorSelector : ComboBox = Control.prepareColorBox ()

    let gridColor : ComboBox = Control.prepareColorBox ()
    let gridCheckBox : CheckBox = Control.prepare CheckBox
    let gridSpacing : IntegerUpDown = Control.prepare IntegerUpDown

    let backgroundColor : ComboBox = Control.prepareColorBox ()

    let deleteButton : MenuItem = Control.prepare' MenuItem 2.0
    let addButton : MenuItem = Control.prepare' MenuItem 2.0

    let figures : Collections.Generic.List<Figure> = Collections.Generic.List()
    
    let mutable figureBuilder : FigureBuilder option = None
    let mutable moveStartPoint : Point option = None
    let mutable movedFigure : int option = None

    let rerender _ =
        use context = new BitmapRenderer(mainCanvas.GetBitmapContext(ReadWriteMode.ReadWrite)) :> IRenderer
        context.Clear (backgroundColor.SelectedValue :?> Color)
        if gridCheckBox.IsChecked.Value then renderGrid context gridSpacing.Value.Value (gridColor.SelectedValue :?> Color)
        renderFigures context figures

    let updateFigure action =
        rerender ()
        match action with
        | FigureModified i ->
            figureDescriptions.[i] <- longDescriptionOf figures.[i]
            figureList.SelectedIndex <- i
        | FigureAdded      ->
            figureDescriptions.Add(longDescriptionOf <| figures.[figures.Count - 1])
        | FigureRemoved  i ->
            figureDescriptions.RemoveAt(i)

    let onSizeChanged (e : SizeChangedEventArgs) =
        mainCanvas <- BitmapFactory.New(int e.NewSize.Width, int e.NewSize.Height)
        mainImage.Source <- mainCanvas
        rerender ()

    let onSelectionChanged (e : SelectionChangedEventArgs) =
        let idx = figureList.SelectedIndex
        deleteButton.IsEnabled <- idx > -1
        if idx > -1 then colorSelector.SelectedValue <- getFigureColor figures.[idx]

    let deleteFigure _ =
        let idx = figureList.SelectedIndex
        figures.RemoveAt(idx)
        updateFigure (FigureRemoved idx)
        ()

    let startBuildingFigure (e : RoutedEventArgs) =
        let figure = (e.Source :?> MenuItem).Tag :?> Figure
        figureBuilder <- Some (getFigureBuilder figure)
        ()

    let imageMouseDown (e : Input.MouseButtonEventArgs) =
        let pos = e.GetPosition mainImage
        let pt = (int pos.X, int pos.Y)
        match figureBuilder with
        | None ->
            let idx = whichFigureWasHit pt figures
            figureList.SelectedIndex <- whichFigureWasHit pt figures
            moveStartPoint <- if idx > -1 then Some pt else None
            movedFigure <- if idx > -1 then Some idx else None
            Input.Mouse.Capture mainImage |> ignore
        | Some _ -> ()

    let imageMouseMove (e : Input.MouseEventArgs) = 
        let pos = e.GetPosition mainImage
        let pt = (int pos.X, int pos.Y)
        match (movedFigure, moveStartPoint) with
        | (Some idx, Some s) ->
            figures.[idx] <- moveFigure (pt -~ s) figures.[idx]
            updateFigure (FigureModified idx)
            moveStartPoint <- Some pt
        | _ -> ()

    let imageMouseUp (e : Input.MouseButtonEventArgs) =
        let pos = e.GetPosition mainImage
        let pt = (int pos.X, int pos.Y)
        let color = colorSelector.SelectedValue :?> Color
        match figureBuilder with
        | Some b -> match processBuildingFigure b pt color with
                    | Choice1Of2 f ->
                        figures.Add f
                        updateFigure FigureAdded
                        figureBuilder <- None
                    | Choice2Of2 b -> figureBuilder <- Some b
        | None -> ()
        moveStartPoint <- None
        movedFigure <- None
        Input.Mouse.Capture null |> ignore

    let updateFigureColor (e : SelectionChangedEventArgs) =
        let color = (e.AddedItems.[0] :?> UIColors.UIColor).Color
        let idx = figureList.SelectedIndex
        if idx > -1
        then
            figures.[idx] <- updateFigureColor color figures.[idx]
            updateFigure (FigureModified idx)
        else ()
    
    do
       base.Title <- "My fancy test window"
       base.WindowStartupLocation <- WindowStartupLocation.CenterScreen
       base.Width <- 1024.0
       base.Height <- 768.0
       
       let mainGrid = Grid()
       [1..6] |> List.map (fun _ -> RowDefinition()) |> List.iter mainGrid.RowDefinitions.Add
       [1..4] |> List.map (fun _ -> ColumnDefinition()) |> List.iter mainGrid.ColumnDefinitions.Add
       mainGrid.RowDefinitions.[0].Height <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.RowDefinitions.[1].Height <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.RowDefinitions.[3].Height <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.RowDefinitions.[4].Height <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.RowDefinitions.[5].Height <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.ColumnDefinitions.[1].Width <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.ColumnDefinitions.[2].Width <- GridLength(1.0, GridUnitType.Auto)
       mainGrid.ColumnDefinitions.[3].Width <- GridLength(1.0, GridUnitType.Auto)

       let infoColumnSpan = 3

       let imageContainer = Control.prepare Grid
       imageContainer.SizeChanged.Add onSizeChanged
       imageContainer.Children.Add mainImage |> ignore
       Grid.SetRowSpan(imageContainer, 5)
       mainImage.HorizontalAlignment <- HorizontalAlignment.Stretch
       mainImage.VerticalAlignment <- VerticalAlignment.Stretch
       mainImage.MouseDown.Add imageMouseDown
       mainImage.MouseMove.Add imageMouseMove
       mainImage.MouseUp.Add imageMouseUp
       Control.positionIn mainGrid imageContainer 0 1
       
       let figLabel = Control.prepare TextBlock
       figLabel.Text <- "Figures:"
       Grid.SetColumnSpan(figLabel, infoColumnSpan)
       Control.positionIn mainGrid figLabel 1 1

       figureList.Width <- 250.0
       figureList.SelectionChanged.Add onSelectionChanged
       figureList.ItemsSource <- figureDescriptions
       Grid.SetColumnSpan(figureList, infoColumnSpan)
       Control.positionIn mainGrid figureList 1 2

       let figColor = Control.prepare TextBlock
       figColor.VerticalAlignment <- VerticalAlignment.Center
       figColor.Text <- "Color:"
       Control.positionIn mainGrid figColor 1 3

       colorSelector.SelectionChanged.Add updateFigureColor
       Grid.SetColumnSpan(colorSelector, infoColumnSpan - 1)
       Control.positionIn mainGrid colorSelector 2 3

       gridCheckBox.Content <- "Grid"
       gridCheckBox.VerticalAlignment <- VerticalAlignment.Center
       gridCheckBox.Unchecked.Add rerender
       gridCheckBox.Checked.Add rerender
       Control.positionIn mainGrid gridCheckBox 1 4

       gridSpacing.Width <- 80.0
       gridSpacing.Value <- Nullable(100)
       gridSpacing.Minimum <- Nullable(5)
       gridSpacing.Maximum <- Nullable(1000)
       gridSpacing.ValueChanged.Add rerender
       Control.positionIn mainGrid gridSpacing 2 4

       gridColor.Width <- 80.0
       gridColor.HorizontalAlignment <- HorizontalAlignment.Right
       gridColor.SelectionChanged.Add rerender
       Control.positionIn mainGrid gridColor 3 4

       let bgColorLabel = Control.prepare TextBlock
       bgColorLabel.Text <- "Background:"
       bgColorLabel.VerticalAlignment <- VerticalAlignment.Center
       Control.positionIn mainGrid bgColorLabel 1 5

       backgroundColor.SelectedValue <- Colors.White
       backgroundColor.SelectionChanged.Add rerender
       Grid.SetColumnSpan(backgroundColor, 2)
       Control.positionIn mainGrid backgroundColor 2 5

       let mainMenu = Menu()
       Grid.SetColumnSpan(mainMenu, infoColumnSpan + 1)

       availableFigures
           |> List.zip (availableFigures |> List.map shortDescriptionOf)
           |> List.map (fun (f, d) ->
               let mi = Control.newMenuItem startBuildingFigure f
               mi.Tag <- d
               mi)
           |> List.iter (addButton.Items.Add >> ignore)
       Control.addMenuItem mainMenu addButton "Add" ignore

       deleteButton.IsEnabled <- false
       Control.addMenuItem mainMenu deleteButton "Delete" deleteFigure

       Control.positionIn mainGrid mainMenu 0 0

       base.Content <- mainGrid