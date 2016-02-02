module FsRaster.Resources

open System.IO
open System.Collections.Generic
open System.Resources
open System.Windows.Input

type private SampleType = SampleType

let private cursorCache = new Dictionary<string, Cursor> ()

let getResourceManagers =
    System.AppDomain.CurrentDomain.GetAssemblies()
    |> Array.filter (fun a -> a.FullName.Contains("FsRaster"))
    |> Array.map (fun a -> new ResourceManager("Resources", a))

let private getSomething getter name (managers : ResourceManager array) =
    managers |> Array.map (fun m ->
        try
            getter m name
        with
            | e -> null
    )
    |> Array.where (isNull >> not)
    |> Array.head

let getObject = getSomething (fun m n -> m.GetObject(n) :?> byte [])
let getString = getSomething (fun m n -> m.GetString(n))

//let loadCursorFile name =
//    match cursorCache.TryGetValue name with
//    | true, c -> c
//    | _ ->
//        let data = getObject name getResourceManagers
//        use stream = new MemoryStream(data)
//        let c = new Cursor(stream)
//        cursorCache.[name] <- c
//        c

let inline loadString name = getString name getResourceManagers
let inline loadStream name = new MemoryStream(getObject name getResourceManagers)