module Tests

open Xunit
open FsUnit.Xunit
open RoundingUpWorkflow

[<Fact>]
let ``Rounding workflow should round intermediate steps`` () =
    let result = 
        RoundingUpBuilder 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }
    
    result |> should (equalWithin 0.001) 0.048

[<Fact>]
let ``Rounding workflow should round initial values`` () =
    let result =
        RoundingUpBuilder 2 {
            let! a = 2.335
            return a
        }
    
    result |> should (equalWithin 0.001) 2.34

[<Fact>]
let ``Should not round exact values`` () =
    let result = 
        RoundingUpBuilder 4 {
            let! a = 2.5
            let! b = 1.0000
            return a + b
        }
    
    result |> should (equalWithin 0.0001) 3.5