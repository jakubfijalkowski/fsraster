module FsRaster.Main

open System
open System.Windows

open FsRaster.UI

[<EntryPoint>]
[<STAThread>]
let main argv =
    let app = new Application()
    let wnd = new MainWindowController()
    app.Run wnd.Window