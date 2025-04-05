module RoundingUpWorkflow

type RoundingUpBuilder(digits: int) =
    member this.Bind(x: float, f: float -> float) =
        System.Math.Round(x, digits) |> f

    member this.Return(x: float) = System.Math.Round(x, digits)