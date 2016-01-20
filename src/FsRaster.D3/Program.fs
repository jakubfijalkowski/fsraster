module FsRaster.D3.Program

open System
open System.Windows

open FsRaster.D3

[<EntryPoint>]
[<STAThread>]
let main argv =
    let app = new Application()
    let wnd = new MainWindowController()
    app.ShutdownMode <- ShutdownMode.OnMainWindowClose
    app.MainWindow <- wnd.Window
    app.Run wnd.Window
