module FibonacciSum

/// Calculates sum of even fibonacci numbers which are less than 1000000
let fibonacciEvenSum () =
    let rec sumStep acc prev1 prev2 =
        let curr = prev1 + prev2
        if curr <= 1000000 then
            if curr % 2 = 0 then
                sumStep (acc + curr) curr prev1
            else
                sumStep acc curr prev1
        else
            acc
    
    sumStep 0 1 1