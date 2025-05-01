module LambdaParser

open LambdaInterpretator
open FParsec
open System.Collections.Generic

let skipSpaces = skipMany (satisfy (fun c -> c = ' ' || c = '\t'))
let pVariable = many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) .>> skipSpaces |>> Variable

let pTerm, pTermRef = createParserForwardedToRef()

let pFactor =
    choice [
        pVariable
        between (pchar '(' .>> skipSpaces) (pchar ')' .>> skipSpaces) pTerm
    ] .>> skipSpaces

let pAbstraction =
    pipe3
        (pchar '\\' .>> skipSpaces)
        (many1 (many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) .>> skipSpaces))
        (pchar '.' .>> skipSpaces >>. pTerm)
        (fun _ args body ->
            List.foldBack (fun arg acc -> Abstraction(arg, acc)) args body) .>> skipSpaces

let pFactorSequence =
    many1 pFactor |>>
    fun factors ->
        List.reduce (fun acc t -> Application(acc, t)) factors

let pApplication = pFactorSequence .>> skipSpaces

do pTermRef.Value <-
    choice [
        attempt pAbstraction
        attempt pApplication
        pFactor
    ] .>> skipSpaces

let pLetDef =
    pipe2
        (pstring "let" >>. skipSpaces >>.  pVariable .>> skipSpaces)
        (pchar '=' >>. skipSpaces >>. pTerm .>> skipSpaces)
        (fun var term -> var, term)

let pProgram =
    pipe2
        (many (pLetDef .>> skipSpaces .>> skipNewline))
        pTerm
        (fun lets mainTerm ->
            let defs = Dictionary<Term, Term>()
            
            lets |> List.iter (fun (varName, term) ->
                defs.[varName] <- term)

            let rec expandLetDefs (term: Term) =
                match term with
                | Variable name when defs.ContainsKey(Variable name) ->
                    defs.[Variable name]
                | Application (term1, term2) ->
                    Application(expandLetDefs term1, expandLetDefs term2)
                | Abstraction (arg, body ) ->
                    Abstraction(arg, expandLetDefs body) // ??????
                | term -> term
            
            let rec fullExpand term =
                printf "\n\n------------\n%A\n---------\n" term
                let expanded = expandLetDefs term
                let res = betaReduction expanded
                printf "%A\n" expanded
                if res = term then term else expandLetDefs res

            fullExpand mainTerm)


let pExpr = skipSpaces >>. pProgram .>> eof

let parse input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg