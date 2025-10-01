module Functions

/// Calculates the factorial of a number.
let factorial n =
    if n < 0 then None 
    elif n = 0 then Some 1
    else Some (Seq.reduce (*) {1 .. n})

/// Counts the nth Fibonacci number.
let fibonacci n =
    if n < 1 then None
    else
        let rec loop a b counter =
            if counter = 2 then Some b
            else loop b (a + b) (counter - 1)
        
        match n with
        | 1 -> Some 0
        | _ -> loop 0 1 n

/// Reverses the list.
let reverse list =
    let rec reverser acc list =
        match list with
        | [] -> acc
        | head :: tail -> reverser (head :: acc) tail
    reverser [] list

/// Finds number in the list.
let find number list =
    let rec finder acc number list=
        match list with
        | [] -> None
        | head :: tail -> if head = number then Some acc else finder (acc + 1) number tail
    finder 0 number list

/// Gets a list of powers of two within the specified bounds.
let getListOfpowersOfTwo fromPower toPower =
    if toPower >= fromPower then
        let rec generatePowers current power =
            if power > toPower then []
            else current :: generatePowers (current * 2.0) (power + 1)
        generatePowers (pown 2.0 fromPower) fromPower
    else []