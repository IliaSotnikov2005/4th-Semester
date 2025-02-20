namespace Introduction

module Functions =
    let factorial n =
        if n < 0 then None 
        elif n = 0 then Some 1
        else Some (Seq.reduce (*) {1 .. n})

    let fibonacci n =
        if n < 1 then None
        else
            let rec loop a b counter =
                if counter = 1 then Some b
                else loop b (a + b) (counter - 1)
            loop 0 1 n

    let reverse lst =
        let rec reverser acc lst =
            match lst with
            | [] -> acc
            | head :: tail -> reverser (head :: acc) tail
        reverser [] lst

    let find n lst =
        let rec finder acc n lst=
            match lst with
            | [] -> None
            | head :: tail -> if head = n then Some acc else finder (acc + 1) n tail
        finder 0 n lst

    let getListOfpowersOfTwo n m =
        if m >= n then
            let rec generatePowers current power =
                if power > m then []
                else current :: generatePowers (current * 2.0) (power + 1)
            generatePowers (pown 2.0 n) n
        else []