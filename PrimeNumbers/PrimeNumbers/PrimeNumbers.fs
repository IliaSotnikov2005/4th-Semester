module PrimeNumbers

let isPrime n =
    if n < 2 then false
    else
        let sqrtN = int (sqrt (float n))
        { 2 .. sqrtN } |> Seq.forall (fun x -> n % x <> 0)

let primeNumbers =
    let rec nextPrime n =
        if isPrime n then n
        else nextPrime (n + 1)
    
    Seq.unfold (fun n -> Some(n, nextPrime (n + 1))) 2