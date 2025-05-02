/// A main module of lambda interpretator.
module LambdaInterpretator

open LambdaInterpretatorCore
open LambdaParser

/// Interpret string with program.
let interpretString lambdaString =
    let defs, AST = parse lambdaString
    let unwrapped = unwrapBinds (defs, AST)
    betaReduction unwrapped

/// Interpret file with program.
let interpretFromFile path =
    let content = System.IO.File.ReadAllText path
    interpretString content