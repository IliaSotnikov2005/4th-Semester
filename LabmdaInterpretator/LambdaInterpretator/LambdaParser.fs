module LambdaParser

open LambdaInterpretator
open FParsec
open System.Collections.Generic

let ws = skipMany (satisfy (fun c -> c = ' ' || c = '\t')) .>> optional (pstring "//" >>. skipRestOfLine false)
let pVariable = many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) .>> ws |>> Variable

let pTerm, pTermRef = createParserForwardedToRef()

let pFactor =
    choice [
        pVariable
        between (pchar '(' .>> ws) (pchar ')' .>> ws) pTerm
    ] .>> ws

let pAbstraction =
    pipe3
        (pchar '\\' .>> ws)
        (many1 (many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) .>> ws))
        (pchar '.' .>> ws >>. pTerm)
        (fun _ args body ->
            List.foldBack (fun arg acc -> Abstraction(arg, acc)) args body) .>> ws

let pFactorSequence =
    many1 pFactor |>>
    fun factors ->
        List.reduce (fun acc t -> Application(acc, t)) factors

let pApplication = pFactorSequence .>> ws

do pTermRef.Value <-
    choice [
        attempt pAbstraction
        attempt pApplication
        pFactor
    ] .>> ws

let pLetDef =
    pipe2
        (pstring "let" >>. ws >>.  pVariable .>> ws)
        (pchar '=' >>. ws >>. pTerm .>> ws)
        (fun var term -> (var, term))

let skipNewline1 = (pchar '\n' <|> pchar '\r') .>> ws

let pProgram =
    pipe2
        (many (pLetDef .>> ws .>> skipNewline1))
        pTerm
        (fun lets mainTerm ->
            let defs = Dictionary<Term, Term>()
            
            lets |> List.iter (fun (varName, term) ->
                defs.[varName] <- term)
            let rec substitute (term: Term) =
                match term with
                | Variable name when defs.ContainsKey(Variable name) -> defs.[Variable name]
                | Application (term1, term2) -> Application(substitute term1, substitute term2)
                | Abstraction (arg, body) -> Abstraction(arg, substitute body)
                | term -> term
            
            substitute mainTerm)


let pExpr = ws >>. pProgram .>> eof

let parse input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg