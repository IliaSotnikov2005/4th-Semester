module Tests

open Xunit
open FsUnit.Xunit
open ParseTree

[<Fact>]
let ``Parse simple addition`` () =
    let tree = Operator(Plus, Operand(3), Operand(5))
    let result = parse tree
    result |> should equal (Some 8)

[<Fact>]
let ``Parse simple subtraction`` () =
    let tree = Operator(Minus, Operand(10), Operand(4))
    let result = parse tree
    result |> should equal (Some 6)

[<Fact>]
let ``Parse simple multiplication`` () =
    let tree = Operator(Multiply, Operand(3), Operand(5))
    let result = parse tree
    result |> should equal (Some 15)

[<Fact>]
let ``Parse simple division`` () =
    let tree = Operator(Divide, Operand(10), Operand(2))
    let result = parse tree
    result |> should equal (Some 5)

[<Fact>]
let ``Parse division by zero`` () =
    let tree = Operator(Divide, Operand(10), Operand(0))
    let result = parse tree
    result |> should equal None

[<Fact>]
let ``Parse complex expression`` () =
    let tree = Operator(Plus, Operand(3), Operator(Multiply, Operand(5), Operand(2)))
    let result = parse tree
    result |> should equal (Some 13)

[<Fact>]
let ``Parse nested operations`` () =
    let tree = Operator(Multiply, Operator(Plus, Operand(2), Operand(3)), Operand(4))
    let result = parse tree
    result |> should equal (Some 20)