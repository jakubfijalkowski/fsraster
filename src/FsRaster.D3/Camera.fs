module FsRaster.D3.Camera

open System.Windows
open System.Windows.Input

open FsRaster.D3.Math

type Camera = { Position : Vector3; Target : Vector3; Up : Vector3; View : Matrix4 }

let defaultCamera = { Position = vec3 0.0 0.0 0.0; Target = vec3 0.0 0.0 1.0; Up = vec3 0.0 1.0 0.0; View = matIdentity }

let inline makeCamera eye target = { Position = eye; Target = target; Up = vec3 0.0 1.0 0.0; View = matIdentity }

let inline moveTo pos camera = { camera with Position = pos }
let inline moveBy diff camera = { camera with Position = camera.Position + diff }

let inline lookAt target camera = { camera with Target = target }
let inline lookBy diff camera = { camera with Target = camera.Target + diff }

let inline updateMatrix camera = { camera with View = matLookAt camera.Position camera.Target camera.Up }

type CameraController(control : FrameworkElement) =

    let mutable camera = makeCamera (vec3 0.0 0.0 1.0) (vec3 0.0 0.0 0.0)
    
    let mutable leftPressed = false
    let mutable rightPressed = false
    let mutable upPressed = false
    let mutable downPressed = false
    let mutable forwardPressed = false
    let mutable backwardPressed = false

    let getMoveCoeff a b = if a then 1.0 else if b then -1.0 else 0.0

    let updateKeys k v =
        match k with
        | Key.A -> leftPressed <- v
        | Key.D -> rightPressed <- v
        | Key.W -> forwardPressed <- v
        | Key.S -> backwardPressed <- v
        | Key.Space -> upPressed <- v
        | Key.LeftCtrl -> downPressed <- v
        | _ -> ()

    let onKeyDown (e : KeyEventArgs) = updateKeys e.Key true
    let onKeyUp (e : KeyEventArgs) = updateKeys e.Key false

    do
        control.KeyDown.Add onKeyDown
        control.KeyUp.Add onKeyUp

    member x.Camera = camera
    member x.Update dt =
        if leftPressed || rightPressed || forwardPressed || backwardPressed || upPressed || downPressed then
            let forwardVec = (camera.Target - camera.Position).Normal
            let rightVec = (cross3 forwardVec camera.Up).Normal

            let forwardCoeff = getMoveCoeff forwardPressed backwardPressed
            let rightCoeff = getMoveCoeff rightPressed leftPressed
            let upCoeff = getMoveCoeff upPressed downPressed

            let moveVec = (forwardVec * forwardCoeff + rightVec * rightCoeff + camera.Up * upCoeff) * dt

            camera <- camera |> moveBy moveVec |> lookBy moveVec

            true
        else
            false