module Queue

/// A type representing queue.
type Queue<'T>() =
    let mutable queue = []

    /// Put value in the queue.
    member this.Enqueue (value: 'T) =
        queue <- queue @ [value]

    /// Get value from the queue.
    member this.Dequeue () =
        match queue with
        | [] -> raise (new System.InvalidOperationException "Queue is empty")
        | head :: tail ->
            queue <- tail
            head