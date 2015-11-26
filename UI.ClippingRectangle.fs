namespace FsRaster.UI

open System.Windows
open System.Windows.Controls

open FsXaml

open FsRaster.Figures
open FsRaster.Utils

type ClipRectMouseMoveType = ClipResize | ClipMove

type ClippingRectangleController(control : FrameworkElement) =

    [<Literal>]
    let MatchDistance = 10
    [<Literal>]
    let MinSize = 3

    let mutable clipRect = None
    let mutable moveData = None
    let requestRender = Event<unit>()

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
                then Some (ClipMove, pos)
                else if distance pos (right, bottom) <= MatchDistance
                then Some (ClipResize, pos)
                else None
            ) clipRect
        e.Handled <- Option.isSome moveData

    let onMouseMove (e : Input.MouseEventArgs) =
        moveData <-
            match moveData with
            | Some (ClipResize, oldPos) ->
                let newPos = getPosition e
                clipRect <- Option.map (resizeRectMin MinSize (newPos -~ oldPos)) clipRect
                Some (ClipResize, newPos)
            | Some (ClipMove, oldPos) ->
                let newPos = getPosition e
                clipRect <- Option.map (moveRect (newPos -~ oldPos)) clipRect
                Some (ClipMove, newPos)
            | None -> None

        if Option.isSome moveData then requestRender.Trigger ()

        let cursor =
            Option.bind (fun (left, top, right, bottom) ->
                let pos = getPosition e
                if distance pos (left, top) <= MatchDistance
                then Some Input.Cursors.SizeAll
                else if distance pos (right, bottom) <= MatchDistance
                then Some Input.Cursors.SizeNWSE
                else None
            ) clipRect
        e.Handled <- Option.isSome cursor
        control.Cursor <- Option.opt Input.Cursors.Arrow cursor

    let onMouseUp (e : Input.MouseEventArgs) =
        e.Handled <- Option.isSome moveData
        moveData <- None

    do
        control.MouseDown.Add onMouseDown
        control.MouseMove.Add onMouseMove
        control.MouseUp.Add onMouseUp

    member x.ClipRect = clipRect

    member x.IsEnabled
        with get() = Option.isSome clipRect
        and  set(value) =
            if value
            then clipRect <- Some (getDefaultClipRect ())
            else clipRect <- None

    member x.RequestRender = requestRender.Publish