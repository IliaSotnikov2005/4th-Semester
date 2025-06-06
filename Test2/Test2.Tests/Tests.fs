module Tests

open NUnit.Framework
open FsUnit
open FibonacciSum
open Queue

[<Test>]
let ``Fibonacci sum should give answer`` () =
    fibonacciEvenSum () |> should equal 1089154

[<Test>]
let ``queue works correct``() =
    let queue = Queue<int>()
    queue.Enqueue 1
    queue.Enqueue 2
    queue.Enqueue 3
    queue.Enqueue 4

    queue.Dequeue() |> should equal 1
    
    queue.Enqueue 6

    queue.Dequeue() |> should equal 2
    queue.Dequeue() |> should equal 3
    queue.Dequeue() |> should equal 4
    queue.Dequeue() |> should equal 6

[<Test>]
let ``queue throws exception if try to dequeue empty queue``() =
    let queue = Queue<int>()
    (fun () -> queue.Dequeue() |> ignore) |> should throw typeof<System.InvalidOperationException>