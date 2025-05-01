open LambdaParser
open LambdaInterpretator


let program1 = "let T = \x y.x
T a b"
let res = parse program1
printf "%A\n\n===========\n\n" res
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