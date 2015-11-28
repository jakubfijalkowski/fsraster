module FsRaster.OctTree

open FsRaster.Utils

[<ReferenceEquality>]
type InternalColor = { R: uint64; G: uint64; B: uint64 }
[<ReferenceEquality>]
type RGBTree =
    { mutable IsLeaf: bool
    ; mutable Color: InternalColor
    ; mutable Frequency: uint64
    ; mutable Children: RGBTree option []
    }

let private getColorIndex pos color =
    let mask = 0x80 >>> pos
    let off = 7 - pos
    (Colors.getR color &&& mask >>> off <<< 2) |||
    (Colors.getG color &&& mask >>> off <<< 1) |||
    (Colors.getB color &&& mask >>> off)

let rec private deepCopyTree tree =
    { IsLeaf = tree.IsLeaf
    ; Color = tree.Color
    ; Frequency = tree.Frequency
    ; Children = Array.map (Option.map deepCopyTree) tree.Children
    }

let private prepareNode leaf =
    { IsLeaf = leaf
    ; Color = { R = 0UL; G = 0UL; B = 0UL }
    ; Frequency = 0UL
    ; Children = if leaf then [||] else Array.replicate 8 None
    }

let private    addNode tree color =
    let rec addNode' pos node =
        if pos = 8
        then
            let r = uint64 <| Colors.getR color
            let g = uint64 <| Colors.getG color
            let b = uint64 <| Colors.getB color
            node.Frequency <- node.Frequency + 1UL
            node.Color <- { R = node.Color.R + r; G = node.Color.G + g; B = node.Color.B + b }
        else
            let idx = getColorIndex pos color
            let childNode =
                match node.Children.[idx] with
                | Some n -> n
                | None ->
                    let n = prepareNode (pos = 7)
                    node.Children.[idx] <- Some n
                    n
            node.Frequency <- node.Frequency + 1UL
            addNode' (pos + 1) childNode |> ignore
        node
    addNode' 0 tree

let rec private getNodesAtLevel level node =
    match level with
    | 0 -> [| node |]
    | 1 -> node.Children |> Array.choose id
    | _ -> node.Children |> Array.choose id |> Array.collect (getNodesAtLevel (level - 1))

let private getLeavesCount tree =
    let rec getLeavesCount' = function
        | x when x.IsLeaf -> 1
        | x               -> x.Children |> Array.choose id |> Array.sumBy getLeavesCount'
    getLeavesCount' tree

let private reduceNode tree node =
    let validChildren = node.Children |> Array.choose id
    let mutable r = 0UL
    let mutable g = 0UL
    let mutable b = 0UL
    // Array.sumBy uses checked arithmetic and we don't want this
    for x in validChildren do
        r <- x.Color.R + r
        g <- x.Color.G + g
        b <- x.Color.B + b
    node.IsLeaf <- true
    node.Children <- [||]
    node.Color <- { R = r; G = g; B = b }
    tree

let private reduceLevel tree count level =
    let nodes = getNodesAtLevel level tree |> Array.sortBy (fun n -> n.Frequency)
    let reduced, nodesToReduce =
        Array.takeWhileState (fun r x ->
            if count - r <= 0 then (r, false)
            else
                let reduced = (x.Children |> Array.choose id |> Array.length) - 1
                (r + reduced, true)
            ) 0 nodes
    Array.fold reduceNode tree nodesToReduce |> ignore
    (tree, count - reduced)

let private tryReduceLevel tree count level =
    if count > 0 then reduceLevel tree count level
    else (tree, count)

let rec private adjustColorsToFrequency node =
    if node.IsLeaf then
        let r = node.Color.R / node.Frequency
        let g = node.Color.G / node.Frequency
        let b = node.Color.B / node.Frequency
        node.Color <- { R = r; G = g; B = b; }
    else node.Children |> Array.choose id |> Array.iter adjustColorsToFrequency

let prepareTree colors =
    let root = prepareNode false
    Array.fold addNode root colors

let reduceTreeInternal tree' finalColors =
    let totalColors = getLeavesCount tree'
    let colorsToReduce = totalColors - finalColors
    if colorsToReduce > 0
    then
        let tree = deepCopyTree tree'
        [ 7 .. -1 .. 0] |> List.fold (fun s -> tryReduceLevel tree s >> snd) colorsToReduce |> ignore
        adjustColorsToFrequency tree
        (true, tree)
    else
        (false, tree')

let reduceTree tree' finalColors =
    reduceTreeInternal tree' finalColors |> snd

let getReplacementColor tree color =
    let rec getReplacementColor' node pos =
        if node.IsLeaf then node.Color
        else
            let idx = getColorIndex pos color
            getReplacementColor' (Option.fromSome node.Children.[idx]) (pos + 1)
    let c = getReplacementColor' tree 0
    Colors.fromRGB (int c.R) (int c.G) (int c.B)

let reduceTreeAndColors tree colors finalColors =
    let reduced, newTree  = reduceTreeInternal tree finalColors
    if reduced
    then Array.map (getReplacementColor newTree) colors
    else colors