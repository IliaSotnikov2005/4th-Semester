module Tests

open Xunit
open FsUnit.Xunit
open ParenthesisSequence

[<Fact>]
let ``Empty string should be balanced`` () =
    checkBracketBalance "" |> should be True

[<Fact>]
let ``Single open bracket should be unbalanced`` () =
    checkBracketBalance "(" |> should be False

[<Fact>]
let ``Single close bracket should be unbalanced`` () =
    checkBracketBalance ")" |> should be False

[<Fact>]
let ``Balanced parentheses should return true`` () =
    checkBracketBalance "()" |> should be True

[<Fact>]
let ``Balanced mixed brackets should return true`` () =
    checkBracketBalance "({[]})" |> should be True

[<Fact>]
let ``Unbalanced mixed brackets should return false`` () =
    checkBracketBalance "({[})" |> should be False

[<Fact>]
let ``Nested unbalanced brackets should return false`` () =
    checkBracketBalance "({[}])" |> should be False

[<Fact>]
let ``Balanced string with non-bracket characters should ignore them`` () =
    checkBracketBalance "(a{bdafdf}aac[dfdfeed]edfdfadf)" |> should be True

[<Fact>]
let ``Unbalanced string with non-bracket characters should return false`` () =
    checkBracketBalance "(afsdaf{bd}cdddd[d}e)" |> should be False

[<Fact>]
let ``Complex balanced string should return true`` () =
    checkBracketBalance "[]{{}()}({[()]})" |> should be True

[<Fact>]
let ``Complex unbalanced string should return false`` () =
    checkBracketBalance "[]{{}()}({[()]}))" |> should be False