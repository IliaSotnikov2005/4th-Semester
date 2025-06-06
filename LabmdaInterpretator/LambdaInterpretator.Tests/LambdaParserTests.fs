module LambdaParserTests

open Xunit
open FsUnit.Xunit
open LambdaParser
open LambdaInterpretatorCore

[<Fact>]
let ``parse variable`` () =
    parse "x" |> snd |> should equal (Variable "x")

[<Fact>]
let ``parse application`` () =
    parse "f x" |> snd |> should equal (Application(Variable "f", Variable "x"))

[<Fact>]
let ``parse nested application`` () =
    parse "f (g x)" |> snd |> should equal (Application(Variable "f", Application(Variable "g", Variable "x")))

[<Fact>]
let ``parse abstraction`` () =
    parse "\\x. x" |> snd |> should equal (Abstraction("x", Variable "x"))

[<Fact>]
let ``parse multi-argument abstraction`` () =
    parse "\\x y. x y" |> snd |> should equal (Abstraction("x", Abstraction("y", Application(Variable "x", Variable "y"))))

[<Fact>]
let ``parse let definition`` () =
    let input = "let id = \\x. x\nid"
    let (defs, term) = parse input
    defs |> should haveLength 1
    defs.[0] |> should equal (Variable "id", Abstraction("x", Variable "x"))
    term |> should equal (Variable "id")

[<Fact>]
let ``parse multiple let definitions`` () =
    let input = "let id = \\x. x\nlet const = \\x y. x\nconst"
    let (defs, term) = parse input
    defs |> should haveLength 2
    defs.[0] |> should equal (Variable "id", Abstraction("x", Variable "x"))
    defs.[1] |> should equal (Variable "const", Abstraction("x", Abstraction("y", Variable "x")))
    term |> should equal (Variable "const")

[<Fact>]
let ``unwrapBinds replaces variables with their definitions`` () =
    let input = "let id = \\x. x\nid"
    let parsed = parse input
    let result = unwrapBinds parsed
    result |> should equal (Abstraction("x", Variable "x"))

[<Fact>]
let ``unwrapBinds handles nested applications`` () =
    let input = "let f = \\x. x\nlet g = \\y. y\nf g"
    let parsed = parse input
    let result = unwrapBinds parsed
    result |> should equal (Application(Abstraction("x", Variable "x"), Abstraction("y", Variable "y")))

[<Fact>]
let ``parse fails on invalid input`` () =
    (fun () -> parse "\\" |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``parse handles complex expression with lets`` () =
    let input = "let id = \\x. x\nlet const = \\x y. x\nconst id id"
    let parsed = parse input
    let result = unwrapBinds parsed
    let expected = Application(
                            Application(Abstraction("x", Abstraction("y", Variable "x")),
                                        Abstraction("x", Variable "x")),
                            Abstraction("x", Variable "x"))
    result |> should equal expected