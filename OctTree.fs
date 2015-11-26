module FsRaster.OctTree

open FsRaster.Utils

type Color = System.Windows.Media.Color

// This really isn't the most functional way, but otherwise I wouldn't know how to make it fast-ish
type RGBTree = { mutable IsLeaf : bool; mutable Color : Color; mutable Frequency: int; mutable Children: RGBTree option array }

let rec deepCopyTree tree =
    { IsLeaf = tree.IsLeaf; Color = tree.Color; Frequency = tree.Frequency; Children = Array.map (Option.map deepCopyTree) tree.Children }

let getColorIndex pos (color : Color) =
    let r = int color.R
    let g = int color.G
    let b = int color.B
    let mask = 1 <<< pos
    (r &&& mask >>> pos <<< 2) ||| (g &&& mask >>> pos <<< 1) ||| (b &&& mask >>> pos)

let mixColor (parentColor : Color) pos idx =
    let r = int parentColor.R
    let g = int parentColor.G
    let b = int parentColor.B
    let newR = r ||| (idx &&& 4 >>> 2 <<< pos)
    let newG = g ||| (idx &&& 2 >>> 1 <<< pos)
    let newB = b ||| (idx &&& 1 <<< pos)
    Color.FromRgb(byte newR, byte newG, byte newB)

let prepareRGBtreeNode leaf color =
    { IsLeaf = leaf
    ; Color = color
    ; Frequency = 0
    ; Children = if leaf then [||] else Array.replicate 8 None
    }

let addColor tree color =
    let rec addColor' (node : RGBTree) pos color =
        if pos = 8
        then
            node.Frequency <- node.Frequency + 1
        else
            let colorIdx = getColorIndex pos color
            let childNode =
                match node.Children.[colorIdx] with
                | Some n -> n
                | None   ->
                    let newNode = prepareRGBtreeNode (pos = 7) (mixColor node.Color pos colorIdx)
                    node.Children.[colorIdx] <- Some newNode
                    newNode
            node.Frequency <- node.Frequency + 1
            addColor' childNode (pos + 1) color
    addColor' tree 0 color
    tree

let rec getLevel level tree =
    match level with
    | 0 -> [ tree ]
    | 1 -> tree.Children |> Array.toList |> List.choose id
    | _ -> tree.Children |> Array.toList |> List.choose id |> List.collect (getLevel (level - 1))

let calculateTotalColors tree =
    let rec calculateTotalColors' level tree =
        if level = 0
        then tree.Children |> Array.choose id |> Array.length
        else tree.Children |> Array.fold (fun s o -> Option.withOpt (sumCount <| level - 1) id o <| s) 0
    and sumCount level tree s = s + calculateTotalColors' level tree
    calculateTotalColors' 7 tree

let getColorSubstitute tree color =
    let rec getColorSubstitute' node pos =
        if node.IsLeaf then node.Color
        else
            let idx = getColorIndex pos color
            let child = Option.fromSome node.Children.[idx]
            getColorSubstitute' child (pos + 1)
    getColorSubstitute' tree 0

let reduceNode tree node =
    let children = node.Children |> Array.choose id
    let sumR = children |> Array.sumBy (fun c -> int c.Color.R * c.Frequency)
    let sumG = children |> Array.sumBy (fun c -> int c.Color.G * c.Frequency)
    let sumB = children |> Array.sumBy (fun c -> int c.Color.B * c.Frequency)
    let newR = sumR / node.Frequency
    let newG = sumG / node.Frequency
    let newB = sumB / node.Frequency
    node.Children <- [||]
    node.IsLeaf <- true
    node.Color <- Color.FromRgb(byte newR, byte newG, byte newB)
    tree

let reduceSingleLevel (tree, count) level =
    if count <= 0 then (tree, 0)
    else
        let nodes =
            getLevel level tree |>
            List.sortBy (fun t -> t.Frequency) |>
            List.takeWhileState (fun (i, r) x ->
                let childrenCount = x.Children |> Array.choose id |> Array.length
                if count <= 0
                then ((i, r), false)
                else ((i + 1, r + childrenCount), true)
                ) (0, 0)
        let nodesReduced = List.sumBy (fun x -> x.Children |> Array.choose id |> Array.length) nodes
        let tree' = List.fold reduceNode tree nodes
        (tree', count - nodesReduced + nodes.Length)

let prepareReductionTree colors =
    let root = (prepareRGBtreeNode false System.Windows.Media.Colors.Black)
    let tree = Array.fold addColor root colors
    let totalColors = calculateTotalColors tree
    (tree, totalColors)

let reducePalette colors (tree, totalColors) targetCount =
    let colorsToDelete = totalColors - targetCount
    let levels = [ 7 .. -1 .. 0 ]
    if colorsToDelete <= 0 then colors
    else
        let tree' = deepCopyTree tree
        let finaltree = List.fold reduceSingleLevel (tree', colorsToDelete) levels |> fst
        colors |> Array.map (getColorSubstitute finaltree)

