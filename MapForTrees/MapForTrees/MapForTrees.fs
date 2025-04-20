/// A module for map for trees.
module MapForTrees

/// A binary tree type.
type BinTree<'a> =
    | Empty
    | Node of 'a * BinTree<'a> * BinTree<'a>

/// A map function for binary trees.
let map f binTree =
    let rec mapStep tree cont =
        match tree with
        | Empty -> cont Empty
        | Node(value, left, right) ->
            let newValue = f value
            mapStep left (fun newLeft ->
                mapStep right (fun newRight ->
                    cont (Node(newValue, newLeft, newRight))))

    mapStep binTree id