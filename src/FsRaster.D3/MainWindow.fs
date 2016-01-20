namespace FsRaster.D3

open System

open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading

open FsXaml

open FsRaster.D3.Math
open FsRaster.D3.CoreRenderer3D

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let sharedLock = new obj()
    
    let window = new MainWindow()
    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)
    let mutable frames = 0

    let point = vec4 0.0 0.0 0.0 1.0
    let world = matTranslate 0.5 0.0 0.0
    let view = matView (vec3 0.0 0.0 1.0) (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)
    let mutable projection = matProjection 90.0 1.0 0.1 100.0

    let fpsTimer = new DispatcherTimer()

    let calculateFps _ =
        let fps = frames
        frames <- 0
        window.fpsLabel.Content <- sprintf "FPS: %d" fps

    let render _ =
        use context = acquireRenderer mainCanvas
        clearColor context 0xff000000
        
        let newPoint = (projection * view * world * point).Normalized
        let x = (newPoint.X + 1.0) / 2.0 * double context.Context.Width
        let y = (newPoint.Y + 1.0) / 2.0 * double context.Context.Height

        putPixel context (int x) (int y) 0xffffffff

        frames <- frames + 1
        ()

    let onSizeChanged (e : SizeChangedEventArgs) =
        let oldW = int e.PreviousSize.Width
        let oldH = int e.PreviousSize.Height
        let newW = int e.NewSize.Width
        let newH = int e.NewSize.Height
        if oldW <> newW || oldH <> newH then
            mainCanvas <- BitmapFactory.New(newW, newH)
            projection <- matProjection 90.0 (e.NewSize.Width / e.NewSize.Height) 0.1 100.0
            window.mainImage.Source <- mainCanvas

    do
        window.imageContainer.SizeChanged.Add onSizeChanged

        fpsTimer.Interval <- TimeSpan.FromSeconds(1.0)
        fpsTimer.Tick.Add calculateFps

        CompositionTarget.Rendering.Add render

        fpsTimer.Start()

    member x.Window = window.Root