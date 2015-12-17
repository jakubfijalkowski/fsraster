namespace FsRaster.UI

open System.Windows
open System.Windows.Controls

open FsXaml

open FsRaster.Figures
open FsRaster.Utils

type RectMouseMoveType = Resize | Move

type SceneRectangleController(control : FrameworkElement) =

    [<Literal>]
    let MatchDistance = 10
    [<Literal>]
    let MinSize = 3

    let mutable rect = None
    let mutable moveData = None
    let requestRender = Event<Rectangle option>()

    let getPosition (e : Input.MouseEventArgs) =
        let pos = e.GetPosition(control)
        (int pos.X, int pos.Y)

    let getDefaultClipRect _ =
        let w = control.ActualWidth
        let h = control.ActualHeight
        let left = w * 0.2
        let right = w * 0.8
        let top = h * 0.2
        let bottom = h * 0.8
        (int left, int top, int right, int bottom)
    
    let onMouseDown (e : Input.MouseEventArgs) =
        moveData <-
            Option.bind (fun (left, top, right, bottom) ->
                let pos = getPosition e
                if distance pos (left, top) <= MatchDistance
                then Some (Move, pos)
                else if distance pos (right, bottom) <= MatchDistance
                then Some (Resize, pos)
                else None
            ) rect
        if Option.isSome moveData then
            e.Handled <- true
            Input.Mouse.Capture control |> ignore

    let onMouseMove (e : Input.MouseEventArgs) =
        moveData <-
            match moveData with
            | Some (Resize, oldPos) ->
                let newPos = getPosition e
                rect <- Option.map (resizeRectMin MinSize (newPos -~ oldPos)) rect
                Some (Resize, newPos)
            | Some (Move, oldPos) ->
                let newPos = getPosition e
                rect <- Option.map (moveRect (newPos -~ oldPos)) rect
                Some (Move, newPos)
            | None -> None

        if Option.isSome moveData then requestRender.Trigger rect

        let cursor =
            Option.bind (fun (left, top, right, bottom) ->
                let pos = getPosition e
                if distance pos (left, top) <= MatchDistance
                then Some Input.Cursors.SizeAll
                else if distance pos (right, bottom) <= MatchDistance
                then Some Input.Cursors.SizeNWSE
                else None
            ) rect
        e.Handled <- Option.isSome cursor
        control.Cursor <- Option.opt null cursor

    let onMouseUp (e : Input.MouseEventArgs) =
        if Option.isSome moveData then
            e.Handled <- true
            if Input.Mouse.Captured = (control :> IInputElement) then Input.Mouse.Capture null |> ignore
        moveData <- None

    do
        control.MouseDown.Add onMouseDown
        control.MouseMove.Add onMouseMove
        control.MouseUp.Add onMouseUp

    member x.Rectangle = rect

    member x.IsEnabled
        with get() = Option.isSome rect
        and  set(value) =
            let oldValue = Option.isSome rect
            if value <> oldValue then
                if value
                then rect <- Some (getDefaultClipRect ())
                else rect <- None
                requestRender.Trigger rect

    member x.RequestRender = requestRender.Publish