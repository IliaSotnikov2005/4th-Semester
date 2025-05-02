/// A module containing lambda interpretator.
module LambdaInterpretator

/// A type representing lambda term.
type Term =
    | Variable of string
    | Application of Term * Term
    | Abstraction of string * Term
    override x.ToString() = sprintf "%A" x 

/// Finds free variables in a term.
let freeVariables term =
    let rec step term acc cont =
        match term with
        | Variable x -> cont (Set.add x acc)
        | Application (term1, term2) -> step term1 acc (fun freeVars1 ->
            step term2 acc (fun freeVars2 ->
                cont (Set.union freeVars1 freeVars2)))
        | Abstraction (variable, term) -> step term acc (fun freeVars -> cont (Set.remove variable freeVars))
    
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
    | Abstraction(v, body) when v = var -> term
    | Abstraction(v, body) when isFree v sub ->
        let newVar = newVariable (Set.union (freeVariables body) (freeVariables sub))
        let newBody = substitude v (Variable newVar) body
        Abstraction(newVar, substitude var sub newBody)
    | Abstraction(v, body) -> Abstraction(v, substitude var sub body)
    | _ -> term

/// Performs beta-reduction.
let rec betaReduction term =
    match term with
    | Variable _ -> term
    | Application(Abstraction(x, body), arg) ->
        betaReduction (substitude x arg body)
    | Application(t1, t2) ->
        let reduced = Application(betaReduction t1, betaReduction t2)
        if reduced <> term then betaReduction reduced else reduced
    | Abstraction(x, body) -> Abstraction(x, betaReduction body)

let rec prettyPrint term =
    match term with
    | Variable name -> printf "%s" name
    | Abstraction (param, body) ->
        printf "\\"
        prettyPrint (Variable param)
        printf "."
        prettyPrint body
    | Application (t1, t2) ->
        printf "("
        prettyPrint t1
        printf " "
        prettyPrint t2
        printf ")"