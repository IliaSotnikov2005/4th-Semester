/// A module containing bracket balance checker.
module ParenthesisSequence

/// Gets pair for given bracket.
let getPair bracket =
    match bracket with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | _ -> failwith "Invalid bracket"

/// Checks whether bracket is open.
let isOpenBracket bracket = bracket = '(' || bracket = '[' || bracket = '{'

/// Checks whether bracket is close.
let isCloseBracket bracket = bracket = ')' || bracket = ']' || bracket = '}'

/// Checks bracket balance.
let rec checkBracketBalance (str: string) =
    let rec check chars (stack: char list) =
        match chars with
        | [] -> stack.IsEmpty
        | head :: tail ->
            if isOpenBracket head then
                check tail (head :: stack)
            elif isCloseBracket head then
                match stack with
                | [] -> false
                | top :: stackTail ->
                    if head = getPair top then
                        check tail stackTail
                    else
                        false
            else
                check tail stack
    check (List.ofSeq str) []