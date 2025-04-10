module Tests

open NUnit.Framework
open FsUnit
open EndlessSequence
open PriorityQueue

[<Test>]
let ``endlessSequence should alternate between 1 and -1`` () =
    endlessSequence |> Seq.take 4 |> should equal [1; -1; 1; -1]

[<Test>]
let ``endlessSequence2 should compute correct values for first 5 elements`` () =
    let expected = [1; -2; 3; -4; 5]
    endlessSequence2 |> Seq.take 5 |> should equal expected

[<Test>]
let ``bypass should return empty list for empty tree``() =
    let tree = Node(4,
        Node(2, Node(1, Empty, Empty), Node(3, Empty, Empty)),
        Node(6, Node(5, Empty, Empty), Node(7, Empty, Empty)))
    
    bypass (fun x -> x > 3) tree |> should equal [4; 5; 6; 7]

[<Test>]
let ``Queue worsk correct``() =
    let queue = PriorityQueue<int>()
    queue.Enqueue 1 1
    queue.Enqueue 2 2
    queue.Enqueue 3 3
    queue.Dequeue() |> should equal 1
    queue.Dequeue() |> should equal 2
    queue.Dequeue() |> should equal 3