module Tests

open Xunit
open FsUnit.Xunit
open ParenthesisSequence

let balancedCases = [
    [| "" |]
    [| "()" |]
    [| "({[]})" |]
    [| "(a{bdafdf}aac[dfdfeed]edfdfadf)" |]
    [| "[]{{}()}({[()]})" |]
]

let unbalancedCases = [
    [| "(" |]
    [| ")" |]
    [| "({[})" |]
    [| "({[}])" |]
    [| "(afsdaf{bd}cdddd[d}e)" |]
    [| "[]{{}()}({[()]}))" |]
]

[<Theory>]
[<MemberData(nameof balancedCases)>]
let ``Should return true for balanced cases`` input =
    checkBracketBalance input |> should be True

[<Theory>]
[<MemberData(nameof unbalancedCases)>]
let ``Should return false for unbalanced cases`` input =
    checkBracketBalance input |> should be False