namespace Lazy

type ThreadSafeLazy<'a>(supplier: unit -> 'a) =
    let mutable evaluatedValue = None
    let mutable supplierOption = Some supplier

    let lockObject = obj ()

    let tryEvaluate () =
        match supplierOption with
        | None -> ()
        | Some supplier ->
            try
                let value = supplier ()
                evaluatedValue <- Some (Ok value)
                supplierOption <- None
            with ex ->
                evaluatedValue <- Some (Error ex)
                supplierOption <- None

    interface ILazy<'a> with
        member this.Get () =
            lock lockObject (fun () -> tryEvaluate ())

            match evaluatedValue with
            | Some (Ok value) -> value
            | Some (Error ex) -> raise ex
            | None -> raise (new System.ArgumentException "Failed to evaluate supplier.")