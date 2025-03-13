module ParenthesisSequence

let getPair bracket =
    match bracket with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | _ -> failwith "Invalid bracket"

let isOpenBracket bracket = bracket = '(' || bracket = '[' || bracket = '{'
let isCloseBracket bracket = bracket = ')' || bracket = ']' || bracket = '}'

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