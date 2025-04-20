/// A parse tree module.
module ParseTree

/// A type representing the sign.
type Sign =
| Plus
| Minus
| Divide
| Multiply

/// A type representing the parse tree.
type ParseTree =
| Operand of number : int
| Operator of Sign * ParseTree * ParseTree

/// Evaluates the value of the parse tree.
let parse parseTree =
    let calculate sign operand1 operand2 =
        match sign with
            | Plus -> Some (operand1 + operand2)
            | Minus -> Some (operand1 - operand2)
            | Divide -> if operand2 <> 0 then Some (operand1 / operand2) else None
            | Multiply -> Some (operand1 * operand2)

    let rec parseStep parseTree cont =
        match parseTree with
        | Operand(number) -> cont (Some number)
        | Operator(sign, operand1, operand2) ->
            parseStep operand1 (fun calculatedOperand1 ->
                parseStep operand2 (fun calculatedOperand2 ->
                match calculatedOperand1, calculatedOperand2 with
                | Some op1, Some op2 -> cont (calculate sign op1 op2)
                | _ -> None))
    
    parseStep parseTree  id