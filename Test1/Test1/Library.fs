module EndlessSequence

// Endless sequences
let endlessSequence = Seq.unfold(fun n -> Some(n, -n)) 1

let endlessSequence2 = 
    Seq.initInfinite (fun index -> 
        let value = endlessSequence |> Seq.item (if index % 2 <> 0 then 1 else 0)
        (index + 1) * value)

// Binary tree type
type BinTree<'a> =
    | Empty
    | Node of 'a * BinTree<'a> * BinTree<'a>

// A function that returns all the elements of the binary tree that satisfy the condition passed as a parameter
let bypass filter binTree =
    let rec bypassStep tree cont =
        match tree with
        | Empty -> cont []
        | Node(value, left, right) ->
            bypassStep left (fun leftResult ->
                bypassStep right (fun rightResult ->
                    let current = if filter value then [value] else []
                    cont (leftResult @ current @ rightResult)))
    bypassStep binTree id

// Print list
let printList list =
    list |> List.iter (printf "%A ")
    printfn ""
