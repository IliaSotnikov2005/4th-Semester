module Tests

open FsUnit.Xunit
open Xunit
open MapForTrees

[<Fact>]
let ``map should return Empty for an Empty tree`` () =
    let tree = Empty
    let result = map (fun x -> x + 1) tree
    match result with
    | Empty -> ()
    | _ -> failwith "Should be empty"

[<Fact>]
let ``map should apply function to a single Node`` () =
    let tree = Node(5, Empty, Empty)
    let result = map (fun x -> x * 2) tree
    result |> should equal (Node(10, Empty, Empty))

[<Fact>]
let ``map should apply function to a complex tree`` () =
    let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = map (fun x -> x + 10) tree
    result |> should equal (Node(11, Node(12, Empty, Empty), Node(13, Empty, Empty)))

[<Fact>]
let ``map should handle nested trees`` () =
    let tree = Node(1, Node(2, Node(3, Empty, Empty), Empty), Node(4, Empty, Empty))
    let result = map (fun x -> x * x) tree
    result |> should equal (Node(1, Node(4, Node(9, Empty, Empty), Empty), Node(16, Empty, Empty)))

[<Fact>]
let ``map should work with strings`` () =
    let tree = Node("a", Node("b", Empty, Empty), Node("c", Empty, Empty))
    let result = map (fun (x: string) -> x.ToUpper()) tree
    result |> should equal (Node("A", Node("B", Empty, Empty), Node("C", Empty, Empty)))
