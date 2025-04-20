/// A module containing calculation on stings workflow.
module CalculationsOnStrings

/// A type representing calculation on strings builder.
type CalculatorOnStringsBuilder() =
    /// Binds.
    member this.Bind(x: string, f: int -> int option) =
        match System.Int32.TryParse x with
        | true, num -> f num
        | false, _ -> None

    /// Returns.
    member this.Return(x: int) =
        Some x

let calculator = CalculatorOnStringsBuilder()