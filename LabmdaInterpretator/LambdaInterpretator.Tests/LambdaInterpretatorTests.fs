module LambdaInterpretatorTests

open Xunit
open FsUnit.Xunit
open LambdaInterpretator

[<Fact>]
let ``interpretator should return I after S K K`` () =
    let skk = "let S = \\x y z.x z (y z)
let K = \\x y.x
S K K"

    let result = (interpretString skk).PrettyPrint ()

    result |> should equal "\\z.z"
    
let ``fst (swap (pair first second)) should return second`` () =
    let fsp = "let T = \\x y.x
let F = \\x y.y
let pair = \\a b c.c a b
let fst = \\p.p T
let snd = \\p.p F
let swap = \\p c.c (snd p) (fst p)
fst(swap(pair first second))"

    let result = (interpretString fsp).PrettyPrint ()

    result |> should equal "second"