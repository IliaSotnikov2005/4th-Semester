module CalculationsOnStrings

type CalculatorOnStringsBuilder() =
    member this.Bind(x: string, f: int -> int option) =
        match System.Int32.TryParse x with
        | true, num -> f num
        | false, _ -> None

    member this.Return(x: int) =
        Some x

let calculator = CalculatorOnStringsBuilder()