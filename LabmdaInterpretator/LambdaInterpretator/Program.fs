open LambdaParser
open LambdaInterpretator

// let program1 = "let T = \\x y.x
// let F = \\x y. y
// let p = \\c t e.c t e
// let fst = p T
// let snd = p F
// let swap = \\p c.c (snd p) (fst p)
// fst (swap (p 1 2))"

let program1 = "let T = \\x y.x
let F = \\x y. y
let p = \\a b f.f a b
let fst = \\p.p T
let snd = \\p.p F
let swap = \\p.\\c.c(snd p)(fst p)
fst(swap (p a b))"
let res = parse program1
printf "\n\n\n\n\\n\n\n\n\n\nn\n%A\n\n===========\n\n" res
let res2 = betaReduction res
printf "%A\n" res2

// let S = Abstraction("x", Abstraction("y", Abstraction("z", 
//     Application(
//         Application(Variable "x", Variable "z"),
//         Application(Variable "y", Variable "z")
//     ))))

// let K = Abstraction("x", Abstraction("y", Variable "x"))

// let testTerm = Application(Application(S, K), K)
// let reduced = betaReduction testTerm
// printfn "Результат: %O" reduced