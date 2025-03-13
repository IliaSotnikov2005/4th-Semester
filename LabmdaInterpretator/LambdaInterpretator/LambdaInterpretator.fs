module LambdaInterpretator

type Term =
| Variable of string
| Application of Term * Term
| Abstraction of string * Term

let freeVariables term =
    let rec step term acc cont =
        match term with
        | Variable x -> cont (Set.add x acc)
        | Application (term1, term2) -> step term1 acc (fun freeVars1 ->
            step term2 acc (fun freeVars2 ->
                cont (Set.union freeVars1 freeVars2)))
        | Abstraction (variable, term) -> step term acc (fun freeVars -> cont (Set.remove variable freeVars))
    
    step term Set.empty id

let isFree variable term = Set.contains variable (freeVariables term)

let newVariable usedVariables =
    let rec nextVariable variable =
        if Set.contains variable usedVariables then nextVariable (variable + "a")
        else variable
    
    nextVariable "a"

let rec substitude variable baseTerm substitutionTerm =
    match baseTerm with
    | Variable x when x = variable -> substitutionTerm
    | Variable _ -> baseTerm
    | Application (term1, term2) -> Application (substitude variable term1 substitutionTerm, substitude variable term2 substitutionTerm)
    | Abstraction (x, body) when x = variable -> baseTerm
    | Abstraction (x, body) when isFree x substitutionTerm ->
        let newVar = newVariable (Set.union (freeVariables body) (freeVariables substitutionTerm))
        let newBody = substitude x body (Variable newVar)
        Abstraction (newVar, substitude variable newBody substitutionTerm)
    | Abstraction (x, body) -> Abstraction (x, substitude variable body substitutionTerm)


let rec betaReduction term =
    match term with
    | Variable _ -> term
    | Application (Abstraction(x, body), argument) ->
        substitude x body argument
    | Application (term1, term2) -> Application (betaReduction term1, betaReduction term2)
    | Abstraction (x, body) -> Abstraction(x, betaReduction body)