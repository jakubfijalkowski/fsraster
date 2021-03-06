﻿namespace FsRaster.D3

open System
open System.Diagnostics
open System.Threading

open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Threading

open Microsoft.Win32

open FsXaml

open FsRaster
open FsRaster.RawRendering
open FsRaster.DoubleBuffer
open FsRaster.D3.Math
open FsRaster.D3.Model
open FsRaster.D3.Camera
open FsRaster.D3.Renderer
open FsRaster.D3.LowLevelRendering

type MainWindow = XAML<"MainWindow.xaml", true>

type MainWindowController() =

    let window = new MainWindow()
    let mutable requestClose = false

    let mutable model = loadOffFromResources "mushroom"

    let mutable mainCanvas : WriteableBitmap = BitmapFactory.New(1, 1)
    let doubleBuffer = makeDoubleBuffer 1 1
    let mutable renderer = defaultRenderer

    let mutable savedCamera = defaultCamera
    let mutable isLightCurrentCamera = false
    let cameraController = CameraController(window.Root)

    let mutable frames = 0
    let mutable lastFpsCheck = 0L
    let mutable lastFrameTime = 0L
    let mutable renderDuration = 0L

    let updateStats () =
        let ticks = DateTime.Now.Ticks
        if ticks - lastFpsCheck > 10000000L then
            let fps = frames
            let frameTime = double renderDuration / double Stopwatch.Frequency * 1000.0 / double frames
            window.fpsLabel.Content <- sprintf "FPS: %d" fps
            window.timeLabel.Content <- sprintf "Render time: %.3f ms" frameTime
            frames <- 0
            lastFpsCheck <- ticks
            renderDuration <- 0L

    let updateCamera dt =
        if cameraController.Update dt then
            renderer <- setCameraTo cameraController.Camera renderer
            if isLightCurrentCamera then
                let newLight = Light.updateLightFromCamera renderer.Light cameraController.Camera
                renderer <- setLightTo newLight renderer
        ()

    let renderLoop () =
        while not requestClose do
            let frameTime = DateTime.Now.Ticks
            let dt = double (frameTime - lastFrameTime) / 10000000.0
            lastFrameTime <- frameTime

            #if DEBUG || PROFILE
            let sw = Stopwatch()
            sw.Start()
            #endif
            updateCamera dt
            updateAndSwap doubleBuffer (fun context ->
                clearBitmap context 0xff000000
                drawModel renderer context model
            )
            #if DEBUG || PROFILE
            sw.Stop()
            renderDuration <- renderDuration + sw.ElapsedTicks
            #endif
            frames <- frames + 1

    let renderThread = Thread(renderLoop)

    let colorizeModel model =
        match window.modelColorModeSelector.SelectedIndex with
        | 0 -> randomlyColorizeModel model
        | 1 -> makeItBlack model
        | _ -> makeItWhite model

    let onCompositionTargetRenderRequest _ =
        updateStats ()

        use screenBuffer = mainCanvas.GetBitmapContext(ReadWriteMode.ReadWrite)
        showOnScreen doubleBuffer screenBuffer.Pixels

    let onSizeChanged (e : SizeChangedEventArgs) =
        let oldW = int e.PreviousSize.Width
        let oldH = int e.PreviousSize.Height
        let newW = int e.NewSize.Width
        let newH = int e.NewSize.Height
        if oldW <> newW || oldH <> newH then
            mainCanvas <- BitmapFactory.New(newW, newH)
            updateBufferSize doubleBuffer newW newH
            renderer <- updateSize renderer newW newH
            window.mainImage.Source <- mainCanvas

    let onCameraChanged _ =
        match window.cameraSelector.SelectedIndex with
        | 0 ->
            cameraController.Camera <- savedCamera
            isLightCurrentCamera <- false
        | _ ->
            savedCamera <- cameraController.Camera
            cameraController.Camera <- Light.lightToCamera renderer.Light
            isLightCurrentCamera <- true

    let onBackfaceCullingToggled _ =
        renderer <- toggleBackfaceCulling renderer

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

    let onWindowClosing _ =
        requestClose <- true
        renderThread.Join()

    let onLoadModelClicked _ =
        let ofd = OpenFileDialog()
        ofd.Filter <- "OFF file (*.off)|*.off"
        ofd.CheckFileExists <- true
        ofd.CheckPathExists <- true
        ofd.ValidateNames <- true
        if ofd.ShowDialog().GetValueOrDefault(false) then
            use stream = ofd.OpenFile()
            try
                model <- loadOffFromStream stream |> colorizeModel
            with
                | e -> MessageBox.Show(window.Root, "Error", "Cannot load OFF file") |> ignore
        ()

    let onChangeOrientationClicked _ =
        model <- changeOrientation model

    do
        window.imageContainer.SizeChanged.Add onSizeChanged
        window.imageContainer.MouseDown.Add onImageClick
        window.loadModelButton.Click.Add onLoadModelClicked
        window.changeOrientationButton.Click.Add onChangeOrientationClicked
        window.modelColorModeSelector.SelectionChanged.Add onColorModeChanged
        window.cameraSelector.SelectionChanged.Add onCameraChanged

        window.wireframeCheckbox.Checked.Add onWireframeToggled
        window.wireframeCheckbox.Unchecked.Add onWireframeToggled

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

        window.Root.Closing.Add onWindowClosing

        model <- model |> colorizeModel
        renderer <- setCameraTo (cameraController.Camera) renderer 

        renderThread.Start()
        CompositionTarget.Rendering.Add onCompositionTargetRenderRequest

    member x.Window = window.Root