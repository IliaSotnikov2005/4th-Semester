open LambdaParser
open LambdaInterpretator

let program1 = "let T = \\x y.x
let F = \\x y. y
let p = \\t e c.c t e
let fst = \\p.p T
let snd = \\p.p F
let swap = \\p c.c (snd p) (fst p)
fst (swap (p first second))"
let res = parse program1
prettyPrint res
printfn "\n"
let res2 = betaReduction res
prettyPrint res2

// let S = Abstraction("x", Abstraction("y", Abstraction("z", 
//     Application(
//         Application(Variable "x", Variable "z"),
//         Application(Variable "y", Variable "z")
//     ))))

// let K = Abstraction("x", Abstraction("y", Variable "x"))

// let testTerm = Application(Application(S, K), K)
// let reduced = betaReduction testTerm
// printfn "Результат: %O" reduced