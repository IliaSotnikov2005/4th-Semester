namespace Lazy
open System.Threading

/// The lock-free version guarantees correct operation in multithreaded mode; calculations are not performed more than once.
type LockFreeLazy<'a>(supplier: unit -> 'a) =
    [<VolatileField>]
    let mutable evaluatedValue = None
    let mutable supplierOption = Some supplier

    let tryEvaluate () =
        let currentSupplier = Interlocked.Exchange(&supplierOption, None)
        
        match currentSupplier with
        | None -> ()
        | Some supplier ->
            try
                let value = supplier ()
                Volatile.Write(&evaluatedValue, Some(Ok value))
            with ex ->
                Volatile.Write(&evaluatedValue, Some (Error ex))

    interface ILazy<'a> with
        member this.Get () =
            tryEvaluate ()
            let rec wait () =
                match Volatile.Read(&evaluatedValue) with
                | Some (Ok value) -> value
                | Some (Error ex) -> raise ex
                | None -> wait ()
            wait ()