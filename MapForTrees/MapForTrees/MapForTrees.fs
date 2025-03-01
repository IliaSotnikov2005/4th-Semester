module MapForTrees

type BinTree<'a> =
    | Empty
    | Node of 'a * BinTree<'a> * BinTree<'a>

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