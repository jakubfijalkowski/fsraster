module FsRaster.Resources

open System.IO
open System.Collections.Generic
open System.Resources
open System.Windows.Input

type private SampleType = SampleType

let private cursorCache = new Dictionary<string, Cursor> ()

let getResourceManager =
    let assembly = System.Reflection.Assembly.GetEntryAssembly()
    new ResourceManager("Resources", assembly)

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

let loadStream name =
    let data = getResourceManager.GetObject(name) :?> byte []
    new MemoryStream(data)