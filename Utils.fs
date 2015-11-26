module FsRaster.Utils

open System.Collections

open System.Windows.Controls
open System.Windows.Media

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)
let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ConsIList =
    type ConsIList<'a> =
        | ConsList of Generic.IList<'a> * int

    let ofList list = ConsList (list, 0)
    let isEmpty (ConsList (list, idx)) = list.Count = idx
    let nextElement (ConsList (list, idx)) = (list.[idx], ConsList(list, idx + 1))

let (|ListNil|ListCons|) lst = if ConsIList.isEmpty lst then ListNil else ListCons (ConsIList.nextElement lst)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module UIColors =
    type UIColor = { Color : Color; Name : string }
    let allColors =
        typeof<Colors>.GetProperties()
        |> Array.map (fun p -> { Color = p.GetValue(null) :?> Color; Name = p.Name })

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module List =
    let rec butLast = function
        | []     -> []
        | [a]    -> []
        | a :: r -> a :: butLast r

    let rec takeWhileState f s lst =
        match lst with
        | x :: xs ->
            let s', r = f s x
            if r then x :: takeWhileState f s' xs else []
        | [] -> []

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Option =
    let opt def o = Option.fold (fun _ -> id) def o
    let optFunc def = function
    | Some o -> o
    | None   -> def ()
    let withOpt f def o = Option.fold (fun _ s -> f s) def o
    let fromSome = function | Some o -> o | _ -> failwith "Invalid argumen - None passed"
