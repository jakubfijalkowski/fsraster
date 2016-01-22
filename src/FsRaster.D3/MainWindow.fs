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

open FsRaster
open FsRaster.RawRendering
open FsRaster.D3.Math
open FsRaster.D3.Model
open FsRaster.D3.Camera
open FsRaster.D3.Renderer
open FsRaster.D3.LowLevelRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let sharedLock = new obj()
    
    let window = new MainWindow()

    let mutable model = loadOffFromResources "mushroom"

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)

    let mutable savedCamera = defaultCamera
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
            if window.cameraSelector.SelectedIndex > 0 then
                let newLight = Light.updateLightFromCamera renderer.Light cameraController.Camera
                renderer <- setLightTo newLight renderer
        ()

    let renderLoop' () =
        let frameTime = DateTime.Now.Ticks
        let dt = double (frameTime - lastFrameTime) / 10000000.0
        lastFrameTime <- frameTime

        updateCamera dt

        use bmpContext = mainCanvas.GetBitmapContext(ReadWriteMode.ReadWrite)
        let context = { Width = bmpContext.Width; Height = bmpContext.Height; Pixels = bmpContext.Pixels }
        clearBitmap context 0xff000000
        drawModel renderer context model
        frames <- frames + 1
        ()

    let renderLoop _ =
    #if DEBUG || PROFILE
        let sw = Stopwatch()
        sw.Start()
        renderLoop' ()
        sw.Stop()
        updateStats sw.ElapsedMilliseconds
    #else
        renderLoop' ()
        updateStats 0
    #endif

    let colorizeModel model =
        match window.modelColorModeSelector.SelectedIndex with
        | 0 -> randomlyColorizeModel model
        | 1 -> makeItBlack model
        | _ -> makeItWhite model

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
            if name = "teapot" then
                model <- newModel |> changeOrientation |> colorizeModel
            else
                model <- newModel |> colorizeModel
        with
            | e -> MessageBox.Show(window.Root, "Cannot load model: " + e.Message, "Error") |> ignore
        ()

    let onCameraChanged _ =
        match window.cameraSelector.SelectedIndex with
        | 0 ->
            cameraController.Camera <- savedCamera
        | _ ->
            savedCamera <- cameraController.Camera
            cameraController.Camera <- Light.lightToCamera renderer.Light

    let onBackfaceCullingToggled _ =
        renderer <- toggleBackfaceCulling renderer

    let onFrustumCullingToggled _ =
        renderer <- toggleFrustumCulling renderer

    let onWireframeToggled _ =
        renderer <- toggleWireframe renderer

    let onZBufferToggled _ =
        renderer <- toggleZBuffer renderer

    let onLightToggled _ =
        renderer <- toggleLight renderer

    let onColorModeChanged _ =
        model <- model |> colorizeModel

    let onImageClick _ =
        window.imageContainer.Focus() |> ignore

    let updateLightProperties _ =
        let ambient = Colors.fromUIColor window.ambientColorPicker.SelectedColor.Value
        let diffuse = Colors.fromUIColor window.diffuseColorPicker.SelectedColor.Value
        let specular = Colors.fromUIColor window.specularColorPicker.SelectedColor.Value
        let newLight = Light.updateLightColors renderer.Light ambient diffuse specular
        renderer <- setLightTo newLight renderer

        let ambCoeff = window.ambientCoefficient.Value.Value
        let diffCoeff = window.diffuseCoefficient.Value.Value
        let specCoeff = window.specularCoefficient.Value.Value
        let shininess = window.shininessCoefficient.Value.Value
        let material = Light.makeMaterial specCoeff diffCoeff ambCoeff shininess
        model <- { model with Material = material }

    do
        window.imageContainer.SizeChanged.Add onSizeChanged
        window.imageContainer.MouseDown.Add onImageClick
        window.modelSelector.SelectionChanged.Add onModelChanged
        window.modelColorModeSelector.SelectionChanged.Add onColorModeChanged
        window.cameraSelector.SelectionChanged.Add onCameraChanged

        window.wireframeCheckbox.Checked.Add onWireframeToggled
        window.wireframeCheckbox.Unchecked.Add onWireframeToggled

        window.frustumCullingCheckbox.Checked.Add onFrustumCullingToggled
        window.frustumCullingCheckbox.Unchecked.Add onFrustumCullingToggled

        window.backfaceCullingCheckbox.Checked.Add onBackfaceCullingToggled
        window.backfaceCullingCheckbox.Unchecked.Add onBackfaceCullingToggled

        window.zBufferCheckbox.Checked.Add onZBufferToggled
        window.zBufferCheckbox.Unchecked.Add onZBufferToggled

        window.lightCheckbox.Checked.Add onLightToggled
        window.lightCheckbox.Unchecked.Add onLightToggled

        window.ambientColorPicker.SelectedColorChanged.Add updateLightProperties
        window.diffuseColorPicker.SelectedColorChanged.Add updateLightProperties
        window.specularColorPicker.SelectedColorChanged.Add updateLightProperties
        window.ambientCoefficient.ValueChanged.Add updateLightProperties
        window.diffuseCoefficient.ValueChanged.Add updateLightProperties
        window.specularCoefficient.ValueChanged.Add updateLightProperties
        window.shininessCoefficient.ValueChanged.Add updateLightProperties

        model <- model |> colorizeModel
        renderer <- setCameraTo (cameraController.Camera) renderer 

        CompositionTarget.Rendering.Add renderLoop

        fpsTimer.Start()

    member x.Window = window.Root