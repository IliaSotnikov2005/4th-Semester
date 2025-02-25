module Tests

open Xunit
open FsUnit.Xunit
open PrimeNumbers

[<Fact>]
let ``isPrime should return false for numbers less than 2`` () =
    isPrime 0 |> should be False
    isPrime 1 |> should be False

[<Fact>]
let ``isPrime should return true for prime numbers`` () =
    isPrime 2 |> should be True
    isPrime 3 |> should be True
    isPrime 5 |> should be True
    isPrime 7 |> should be True
    isPrime 11 |> should be True

[<Fact>]
let ``isPrime should return false for non-prime numbers`` () =
    isPrime 4 |> should be False
    isPrime 6 |> should be False
    isPrime 8 |> should be False
    isPrime 9 |> should be False
    isPrime 10 |> should be False

[<Fact>]
let ``primeNumbers should generate the first 5 prime numbers`` () =
    primeNumbers
    |> Seq.take 5
    |> Seq.toList
    |> should equal [2; 3; 5; 7; 11]

[<Fact>]
let ``primeNumbers should generate primes greater than 100`` () =
    primeNumbers
    |> Seq.skipWhile (fun x -> x <= 100)
    |> Seq.take 5
    |> Seq.toList
    |> should equal [101; 103; 107; 109; 113]