module LambdaInterpretator

open LambdaInterpretatorCore
open LambdaParser

let interpretString lambdaString =
    let AST = parse lambdaString
    betaReduction AST

let interpretFromFile path =
    let content = System.IO.File.ReadAllText path
    interpretString content