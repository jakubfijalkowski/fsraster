namespace FsRaster.D3

open System

open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading

open FsXaml

open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Models
open FsRaster.D3.Renderer
open FsRaster.D3.LowLevelRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let sharedLock = new obj()
    
    let window = new MainWindow()

    let model = sampleTriangle

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)
    let mutable renderer = defaultRenderer
    let mutable frames = 0

    let fpsTimer = new DispatcherTimer()

    let calculateFps _ =
        let fps = frames
        frames <- 0
        window.fpsLabel.Content <- sprintf "FPS: %d" fps

    let render _ =
        use context = acquireRenderer mainCanvas
        clearBitmap context 0xff000000
        drawModel renderer context model
        frames <- frames + 1
        ()

    let onSizeChanged (e : SizeChangedEventArgs) =
        let oldW = int e.PreviousSize.Width
        let oldH = int e.PreviousSize.Height
        let newW = int e.NewSize.Width
        let newH = int e.NewSize.Height
        if oldW <> newW || oldH <> newH then
            mainCanvas <- BitmapFactory.New(newW, newH)
            renderer <- updateProjection renderer newW newH
            window.mainImage.Source <- mainCanvas

    do
        renderer <- lookAt renderer (vec3 0.0 0.0 1.0) (vec3 0.0 0.0 0.0)

        window.imageContainer.SizeChanged.Add onSizeChanged

        fpsTimer.Interval <- TimeSpan.FromSeconds(1.0)
        fpsTimer.Tick.Add calculateFps

        CompositionTarget.Rendering.Add render

        fpsTimer.Start()

    member x.Window = window.Root