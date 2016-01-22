module FsRaster.D3.Camera

open System.Windows
open System.Windows.Input

open FsRaster.D3.Math

type Camera = { Position : Vector3; Target : Vector3; Up : Vector3; View : Matrix4 }

let defaultCamera = { Position = vec3 0.0 0.0 0.0; Target = vec3 0.0 0.0 1.0; Up = vec3 0.0 1.0 0.0; View = matIdentity }

let inline makeCamera eye target = { defaultCamera with Position = eye; Target = target; Up = vec3 0.0 1.0 0.0 }

let inline moveTo pos camera = { camera with Position = pos }
let inline moveBy diff camera = { camera with Position = camera.Position + diff }

let inline lookAt target camera = { camera with Target = target }
let inline lookBy diff camera = { camera with Target = camera.Target + diff }

let inline updateMatrix camera =
    let view = matLookAt camera.Position camera.Target camera.Up
    { camera with View = view }

type CameraController(control : FrameworkElement) =

    [<Literal>]
    let RotAngle = 1.57079632679 // PI / 2, but I can't use Math.PI / 2.0 in Literals...

    let mutable camera = makeCamera (vec3 0.0 0.0 8.0) (vec3 0.0 0.0 0.0)
    
    let mutable leftPressed = false
    let mutable rightPressed = false
    let mutable upPressed = false
    let mutable downPressed = false
    let mutable forwardPressed = false
    let mutable backwardPressed = false
    let mutable realUpPressed = false
    let mutable realDownPressed = false

    let mutable lookUpPressed = false
    let mutable lookDownPressed = false
    let mutable lookLeftPressed = false
    let mutable lookRightPressed = false

    let getMoveCoeff a b = if a then 1.0 else if b then -1.0 else 0.0

    let moveCamera dt =
        let forwardVec = (camera.Target - camera.Position).Normalized
        let rightVec = (cross3 forwardVec camera.Up).Normalized
        let realUpVec = (cross3 rightVec forwardVec).Normalized

        let forwardCoeff = getMoveCoeff forwardPressed backwardPressed
        let rightCoeff = getMoveCoeff rightPressed leftPressed
        let upCoeff = getMoveCoeff upPressed downPressed
        let realUpCoeff = getMoveCoeff realUpPressed realDownPressed

        let moveVec = (forwardVec * forwardCoeff + rightVec * rightCoeff + camera.Up * upCoeff + realUpVec * realUpCoeff) * dt

        camera <- camera |> moveBy moveVec |> lookBy moveVec

    let rotateCamera dt =
        let forwardVec = (camera.Target - camera.Position).Normalized
        let rightVec = (cross3 forwardVec camera.Up).Normalized
        let realUpVec = (cross3 forwardVec rightVec).Normalized
        let rightAngle = (getMoveCoeff lookRightPressed lookLeftPressed) * RotAngle * dt
        let upAngle = (getMoveCoeff lookUpPressed lookDownPressed) * RotAngle * dt

        let newTarget = forwardVec |> vecRotate rightAngle realUpVec |> vecRotate upAngle rightVec
        camera <- lookAt (camera.Position + newTarget) camera
        ()

    let shouldMove () = leftPressed || rightPressed || forwardPressed || backwardPressed || upPressed || downPressed || realUpPressed || realDownPressed
    let shouldRotate () = lookLeftPressed || lookRightPressed || lookUpPressed || lookDownPressed

    let updateKeys k v =
        match k with
        | Key.A -> leftPressed <- v
        | Key.D -> rightPressed <- v
        | Key.W -> forwardPressed <- v
        | Key.S -> backwardPressed <- v
        | Key.E -> realUpPressed <- v
        | Key.Q -> realDownPressed <- v
        | Key.Space -> upPressed <- v
        | Key.LeftCtrl -> downPressed <- v
        | Key.NumPad4 -> lookLeftPressed <- v
        | Key.NumPad6 -> lookRightPressed <- v
        | Key.NumPad8 -> lookUpPressed <- v
        | Key.NumPad2 -> lookDownPressed <- v
        | _ -> ()

    let onKeyDown (e : KeyEventArgs) = updateKeys e.Key true
    let onKeyUp (e : KeyEventArgs) = updateKeys e.Key false

    do
        control.KeyDown.Add onKeyDown
        control.KeyUp.Add onKeyUp

    member x.Camera = camera
    member x.Update dt =
        if shouldMove () then moveCamera dt
        if shouldRotate () then rotateCamera dt
        shouldMove () || shouldRotate ()