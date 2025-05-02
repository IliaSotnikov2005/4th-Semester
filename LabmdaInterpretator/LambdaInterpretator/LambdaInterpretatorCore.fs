/// A module containing lambda interpretator.
module LambdaInterpretatorCore

/// A type representing lambda term.
type Term =
    | Variable of string
    | Application of Term * Term
    | Abstraction of string * Term

    /// Gives a string with the term as it is.
    override this.ToString() = sprintf "%A" this

    /// Gives a formatted string in the usual form.
    member this.PrettyPrint() =
        let rec format term precedence =
            match term with
            | Variable name -> name
            
            | Application (left, right) ->
                let leftStr = format left 1
                let rightStr = format right 2
                let appStr = $"{leftStr} {rightStr}"
                if precedence > 1 then $"({appStr})" else appStr
            
            | Abstraction (param, body) ->
                let bodyStr = format body 0
                let absStr = $"\\{param}.{bodyStr}"
                if precedence > 0 then $"({absStr})" else absStr
        
        format this 0

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