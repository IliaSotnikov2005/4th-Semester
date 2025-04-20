/// A module containing rounding up workflow.
module RoundingUpWorkflow

/// A type representing rounding up builder.
type RoundingUpBuilder(digits: int) =

    /// Binds.
    member this.Bind(x: float, f: float -> float) =
        System.Math.Round(x, digits) |> f

    /// Returns.
    member this.Return(x: float) = System.Math.Round(x, digits)