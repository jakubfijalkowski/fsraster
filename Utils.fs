module FsRaster.Utils

open System.Collections

open System.Windows.Controls
open System.Windows.Media

let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)

module ConsList =
    type ConsList<'a> =
        | ConsList of Generic.IList<'a> * int

    let ofList list = ConsList (list, 0)
    let isEmpty (ConsList (list, idx)) = list.Count = idx
    let nextElement (ConsList (list, idx)) = (list.[idx], ConsList(list, idx + 1))

let (|ListNil|ListCons|) lst = if ConsList.isEmpty lst then ListNil else ListCons (ConsList.nextElement lst)

module UIColors =
    type UIColor = { Color : Color; Name : string }
    let allColors =
        typeof<Colors>.GetProperties()
        |> Array.map (fun p -> { Color = p.GetValue(null) :?> Color; Name = p.Name })
