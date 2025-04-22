namespace Lazy

/// The version with a guarantee of correct operation in single-threaded mode (without synchronization).
type SingleThreadLazy<'a>(supplier: unit -> 'a) =
    let mutable evaluatedValue = None
    let mutable supplierOption = Some supplier

    interface ILazy<'a> with
        member this.Get () =
            match evaluatedValue with
            | Some (Ok value) -> value
            | Some (Error ex) -> raise ex
            | None ->
                match supplierOption with
                | Some supplier ->
                    try
                        let value = supplier ()
                        evaluatedValue <- Some (Ok value)
                        supplierOption <- None
                        value
                    with ex ->
                        evaluatedValue <- Some (Error ex)
                        supplierOption <- None
                        raise ex
                | None -> failwith "Supplier was already used"