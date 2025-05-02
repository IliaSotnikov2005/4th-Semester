/// A module containing lambda interpretator.
module LambdaInterpretatorCore

/// A type representing lambda term.
type Term =
    | Variable of string
    | Application of Term * Term
    | Abstraction of string * Term
    override x.ToString() = sprintf "%A" x 

    member x.PrettyPrint() =
        let rec printTerm term (needsParens: bool) =
            match term with
            | Variable name -> name
            
            | Abstraction (param, body) ->
                let bodyStr = printTerm body false
                let absStr = $"\\{param}.{bodyStr}"
                if needsParens then $"({absStr})" else absStr
            
            | Application (t1, t2) ->
                let t1Str = printTerm t1 true
                let t2Str = printTerm t2 true
                let appStr = $"{t1Str} {t2Str}"
                if needsParens then $"({appStr})" else appStr
        
        printTerm x false

/// Finds free variables in a term.
let freeVariables term =
    let rec step term acc cont =
        match term with
        | Variable v -> cont (Set.add v acc)
        | Application (term1, term2) -> step term1 acc (fun freeVars1 ->
            step term2 acc (fun freeVars2 ->
                cont (Set.union freeVars1 freeVars2)))
        | Abstraction (param, term) -> step term acc (fun freeVars -> cont (Set.remove param freeVars))
    
    step term Set.empty id

/// Checks whether variable is free in term.
let isFree variable term = Set.contains variable (freeVariables term)

/// Gets a new variable name.
let newVariable usedVariables =
    let rec nextVariable variable =
        if Set.contains variable usedVariables then nextVariable (variable + "a")
        else variable
    
    nextVariable "a"

/// Performs substitude.
let rec substitude var sub term =
    match term with
    | Variable v when v = var -> sub
    | Application(t1, t2) -> 
        Application(substitude var sub t1, substitude var sub t2)
    | Abstraction(param, body) when param = var -> term
    | Abstraction(param, body) when isFree param sub ->
        let newVar = newVariable (Set.union (freeVariables body) (freeVariables sub))
        let newBody = substitude param (Variable newVar) body
        Abstraction(newVar, substitude var sub newBody)
    | Abstraction(param, body) -> Abstraction(param, substitude var sub body)
    | _ -> term

/// Performs beta-reduction.
let rec betaReduction term =
    match term with
    | Variable _ -> term
    | Application(Abstraction(param, body), arg) ->
        betaReduction (substitude param arg body)
    | Application(t1, t2) ->
        let reduced = Application(betaReduction t1, betaReduction t2)
        if reduced <> term then betaReduction reduced else reduced
    | Abstraction(x, body) -> Abstraction(x, betaReduction body)