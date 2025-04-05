module Tests

open NUnit.Framework
open FsUnit
open CalculationsOnStrings

[<Test>]
let ``Valid numbers should return sum`` () =
    let result = calculator {
        let! a = "121"
        let! b = "14"
        return a + b
    }
    result |> should equal (Some 135)

[<Test>]
let ``Invalid first number should return None`` () =
    let result = calculator {
        let! a = "abc"
        let! b = "14"
        return a + b
    }
    result |> should equal None

[<Test>]
let ``Invalid second number should return None`` () =
    let result = calculator {
        let! a = "121"
        let! b = "1x4"
        return a + b
    }
    result |> should equal None

[<Test>]
let ``Both invalid numbers should return None`` () =
    let result = calculator {
        let! a = "12.5"
        let! b = "14z"
        return a + b
    }
    result |> should equal None