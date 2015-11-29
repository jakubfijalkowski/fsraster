module FsRaster.Resources

open System.IO
open System.Collections.Generic
open System.Resources
open System.Windows.Input

type private SampleType = SampleType

let private cursorCache = new Dictionary<string, Cursor> ()

let getResourceManager =
    new ResourceManager("Resources", typeof<SampleType>.Assembly)

let loadCursorFile name =
    match cursorCache.TryGetValue name with
    | true, c -> c
    | _ ->
        let data = getResourceManager.GetObject(name) :?> byte []
        use stream = new MemoryStream(data)
        let c = new Cursor(stream)
        cursorCache.[name] <- c
        c

let loadString name =
    getResourceManager.GetString(name)