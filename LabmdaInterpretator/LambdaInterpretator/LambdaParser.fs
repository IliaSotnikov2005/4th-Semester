/// A module containing labmda expression parser with let defenitions.
module LambdaParser

open LambdaInterpretatorCore
open FParsec
open System.Collections.Generic

/// Skip chars ' ', '\t'.
let skipSpaces = skipMany (satisfy (fun c -> c = ' ' || c = '\t'))

/// Parses variable according to var ::= [a-z]+.
let pVariable = 
    many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) 
    .>> skipSpaces 
    >>= fun s -> 
        if s.ToLower() = "let" then 
            raise (new System.ArgumentException "Variable name 'let' is reserved.")
        else 
            preturn (Variable s)

/// Parses term according to term ::= abstraction | application | factor.
let pTerm, pTermRef = createParserForwardedToRef()

/// Parses factor according to factor ::= var | (term).
let pFactor =
    choice [
        pVariable
        between (pchar '(' .>> skipSpaces) (pchar ')' .>> skipSpaces) pTerm
    ] .>> skipSpaces

/// Parses abstraction according to abstraction ::= \var_seq.term.
let pAbstraction =
    pipe3
        (pchar '\\' .>> skipSpaces)
        (many1 (many1Chars (anyOf (['a'..'z'] @ ['A'..'Z'])) .>> skipSpaces))
        (pchar '.' .>> skipSpaces >>. pTerm)
        (fun _ args body ->
            List.foldBack (fun arg acc -> Abstraction(arg, acc)) args body) .>> skipSpaces

/// Parses factor sequence according to factor_seq ::= factor factor_seq | factor.
let pFactorSequence =
    many1 pFactor |>>
    fun factors ->
        List.reduce (fun acc t -> Application(acc, t)) factors

/// Parses application according to application ::= factor factor_seq.
let pApplication = pFactorSequence .>> skipSpaces

// Parser realisation for pTerm.
do pTermRef.Value <-
    choice [
        attempt pAbstraction
        attempt pApplication
        pFactor
    ] .>> skipSpaces

/// Parses bind (let defenition) according to bind ::= "let" var "=" term.
let pBind =
    tuple2
        (pstring "let" >>. skipSpaces >>.  pVariable .>> skipSpaces)
        (pchar '=' >>. skipSpaces >>. pTerm .>> skipSpaces)
/// Parses program according to program ::= bind program | term.
let pProgram =
    tuple2
        (many (pBind .>> skipSpaces .>> many1 skipNewline))
        pTerm

/// Unwraps all binds in term. 
let unwrapBinds (lets: (Term * Term) list, mainTerm: Term) =
    let binds = Dictionary<Term, Term>()
    
    lets |> List.iter (fun (varTerm, term) ->
        binds.[varTerm] <- term)

    let rec unwrap (term: Term) (boundVars: Set<string>) =
        match term with
        | Variable name when binds.ContainsKey(Variable name) && not (boundVars.Contains name) ->
            unwrap binds.[Variable name] boundVars
        | Application (term1, term2) ->
            Application (unwrap term1 boundVars, unwrap term2 boundVars)
        | Abstraction (param, body) ->
            let newBoundVars = Set.add param boundVars
            Abstraction (param, unwrap body newBoundVars)
        | term -> term

    unwrap mainTerm Set.empty

/// Auxiliary parser for pProgram.
let pExpr = spaces >>. pProgram .>> spaces .>> eof

/// Main parse function.
let parse input =
    match run pExpr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Parse error: %s" errorMsg