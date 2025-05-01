module LambdaParser

open LambdaInterpretator
open FParsec

let ws = spaces
let pVariable = many1Chars (anyOf ['a'..'z']) .>> ws |>> Variable

let pTerm, pTermRef = createParserForwardedToRef()

let pFactor =
    choice [
        pVariable
        between (pchar '(' .>> ws) (pchar ')' .>> ws) pTerm
    ] .>> ws

let pAbstraction =
    pipe3
        (pchar '\\' .>> ws)
        (many1Chars (anyOf ['a'..'z']) .>> ws)
        (pchar '.' .>> ws >>. pTerm)
        (fun _ param body -> Abstraction(param, body)) .>> ws

let pFactorSequence =
    many1 pFactor |>>
    fun factors ->
        List.reduce (fun acc t -> Application(acc, t)) factors

let pApplication = pFactorSequence .>> ws

do pTermRef :=
    choice [
        attempt pAbstraction
        attempt pApplication
        pFactor
    ] .>> ws

let pExpr = ws >>. pTerm .>> eof

let parse input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg