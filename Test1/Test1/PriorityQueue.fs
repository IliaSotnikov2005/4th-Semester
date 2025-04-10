module PriorityQueue

// Prioryti queue type
type PriorityQueue<'T>() =
    let mutable queue = []

    member this.Enqueue (value: 'T) (priority: int) =
        queue <- (priority, value) :: queue |> List.sortBy fst

    // throws System.Exception if dequeue from empty queue
    member this.Dequeue() =
        match queue with
        | (priority, value) :: tail ->
            queue <- tail
            value
        | [] -> failwith "Queue is empty!"

    member this.IsEmpty = queue.IsEmpty