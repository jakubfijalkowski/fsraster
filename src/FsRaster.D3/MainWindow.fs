namespace FsRaster.D3

open System
open System.Diagnostics

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
open FsRaster.D3.Camera
open FsRaster.D3.Renderer
open FsRaster.D3.LowLevelRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let sharedLock = new obj()
    
    let window = new MainWindow()

    let mutable model = loadOffFromResources "mushroom" |> changeOrientation

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)

    let cameraController = CameraController(window.Root)

    let mutable renderer = defaultRenderer
    let mutable frames = 0

    let mutable lastFpsCheck = 0L
    let mutable lastFrameTime = 0L

    let fpsTimer = new DispatcherTimer()

    let updateStats rt =
        let ticks = DateTime.Now.Ticks
        if ticks - lastFpsCheck > 10000000L then
            let fps = frames
            window.fpsLabel.Content <- sprintf "FPS: %d" fps
            window.timeLabel.Content <- sprintf "Render time: %d ms" (int rt)
            frames <- 0
            lastFpsCheck <- ticks

    let updateCamera dt =
        if cameraController.Update dt then
            renderer <- setCameraTo cameraController.Camera renderer
        ()

    let renderLoop' () =
        let frameTime = DateTime.Now.Ticks
        let dt = double (frameTime - lastFrameTime) / 10000000.0
        lastFrameTime <- frameTime

        updateCamera dt

        use context = acquireRenderer mainCanvas
        clearBitmap context 0xff000000
        drawModel renderer context model
        frames <- frames + 1
        ()

    let renderLoop _ =
    #if DEBUG
        let sw = Stopwatch()
        sw.Start()
        renderLoop' ()
        sw.Stop()
        updateStats sw.ElapsedMilliseconds
    #else
        renderLoop' ()
        updateStats 0
    #endif

    let onSizeChanged (e : SizeChangedEventArgs) =
        let oldW = int e.PreviousSize.Width
        let oldH = int e.PreviousSize.Height
        let newW = int e.NewSize.Width
        let newH = int e.NewSize.Height
        if oldW <> newW || oldH <> newH then
            mainCanvas <- BitmapFactory.New(newW, newH)
            renderer <- updateSize renderer newW newH
            window.mainImage.Source <- mainCanvas

    let onModelChanged _ =
        try
            let name = (window.modelSelector.SelectedItem :?> ComboBoxItem).Tag :?> string
            let newModel = loadOffFromResources name
            if name = "mushroom" then
                model <- newModel |> changeOrientation
            else
                model <- newModel
        with
            | e -> MessageBox.Show(window.Root, "Cannot load model: " + e.Message, "Error") |> ignore
        ()

    let onBackfaceCullingToggled _ =
        renderer <- toggleBackfaceCulling renderer

    let onWireframeToggled _ =
        renderer <- toggleWireframe renderer

    let onZBufferToggled _ =
        renderer <- toggleZBuffer renderer

    let onImageClick _ =
        window.imageContainer.Focus() |> ignore

    do
        window.imageContainer.SizeChanged.Add onSizeChanged
        window.imageContainer.MouseDown.Add onImageClick
        window.modelSelector.SelectionChanged.Add onModelChanged

        window.wireframeCheckbox.Checked.Add onWireframeToggled
        window.wireframeCheckbox.Unchecked.Add onWireframeToggled

        window.backfaceCullingCheckbox.Checked.Add onBackfaceCullingToggled
        window.backfaceCullingCheckbox.Unchecked.Add onBackfaceCullingToggled

        window.zBufferCheckbox.Checked.Add onZBufferToggled
        window.zBufferCheckbox.Unchecked.Add onZBufferToggled

        renderer <- setCameraTo (cameraController.Camera) renderer 

        CompositionTarget.Rendering.Add renderLoop

        fpsTimer.Start()

    member x.Window = window.Root