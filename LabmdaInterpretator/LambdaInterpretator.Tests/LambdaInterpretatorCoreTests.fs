module Tests

open Xunit
open FsUnit.Xunit
open LambdaInterpretatorCore

[<Fact>]
let ``freeVariables should return correct set for Variable`` () =
    let term = Variable "x"
    freeVariables term |> should equal (Set.ofList ["x"])

[<Fact>]
let ``freeVariables should return correct set for Application`` () =
    let term = Application (Variable "x", Variable "y")
    freeVariables term |> should equal (Set.ofList ["x"; "y"])

[<Fact>]
let ``freeVariables should return correct set for Abstraction`` () =
    let term = Abstraction ("x", Variable "y")
    freeVariables term |> should equal (Set.ofList ["y"])

[<Fact>]
let ``isFree should return true if variable is free`` () =
    let term = Application (Variable "x", Variable "y")
    isFree "x" term |> should be True

[<Fact>]
let ``isFree should return false if variable is not free`` () =
    let term = Abstraction ("x", Variable "y")
    isFree "x" term |> should be False

[<Fact>]
let ``newVariable should return a new variable not in usedVariables`` () =
    let usedVariables = Set.ofList ["a"; "b"; "c"]
    let newVar = newVariable usedVariables
    usedVariables |> should not' (contain newVar)

[<Fact>]
let ``substitude should replace variable in Variable term`` () =
    let term = Variable "x"
    let result = substitute "x" (Variable "y") term
    result |> should equal (Variable "y")

[<Fact>]
let ``substitude should not replace variable in Application term if not matching`` () =
    let term = Application (Variable "x", Variable "y")
    let result = substitute "z" (Variable "w") term
    result |> should equal term

[<Fact>]
let ``substitude should replace variable in Abstraction term is free`` () =
    let term = Abstraction ("x", Variable "y")
    let result = substitute "y" (Variable "z") term
    result |> should equal (Abstraction ("x", Variable "z"))

[<Fact>]
let ``betaReduction should reduce Application of Abstraction`` () =
    let term = Application (Abstraction ("x", Variable "x"), Variable "y")
    let result = betaReduction term
    result |> should equal (Variable "y")

[<Fact>]
let ``betaReduction should not change Variable term`` () =
    let term = Variable "x"
    let result = betaReduction term
    result |> should equal term

[<Fact>]
let ``betaReduction should reduce nested Application terms`` () =
    let term = Application (Application (Abstraction ("x", Variable "x"), Variable "y"), Variable "z")
    let result = betaReduction term
    result |> should equal (Application (Variable "y", Variable "z"))