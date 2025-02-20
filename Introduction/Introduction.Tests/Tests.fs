module Introduction.Tests

open NUnit.Framework
open Introduction.Functions

[<TestFixture>]
type FactorialTests() =

    [<Test>]
    member this.``Factorial of negative number should return None``() =
        let result = factorial -3
        Assert.That(result, Is.EqualTo(None))

    [<Test>]
    member this.``Factorial of 0 should return Some 1``() =
        let result = factorial 0
        Assert.That(result, Is.EqualTo(Some 1))

    [<Test>]
    member this.``Factorial of 5 should return Some 120``() =
        let result = factorial 5
        Assert.That(result, Is.EqualTo(Some 120))

    [<Test>]
    member this.``Factorial of 3 should return Some 6``() =
        let result = factorial 3
        Assert.That(result, Is.EqualTo(Some 6))

[<TestFixture>]
type FibonacciTests() =
    [<Test>]
    member this.``Fibonacci of negative number should return None``() =
        let result = fibonacci -4
        Assert.That(result, Is.EqualTo(None))

    [<Test>]
    member this.``Fibonacci of 6 should return Some 8``() =
        let result = fibonacci 6
        Assert.That(result, Is.EqualTo(Some 8))

[<TestFixture>]
type ReverseTests() =

    [<Test>]
    member this.``Reverse of empty list should return empty list``() =
        let result = reverse []
        Assert.That(result, Is.EqualTo([]))

    [<Test>]
    member this.``Reverse of single-element list should return the same list``() =
        let result = reverse [1]
        Assert.That(result, Is.EqualTo([1]))

    [<Test>]
    member this.``Reverse of multiple-element list should return reversed list``() =
        let result = reverse [1; 2; 3; 4]
        Assert.That(result, Is.EqualTo([4; 3; 2; 1]))

    [<Test>]
    member this.``Reverse of list with duplicate elements should return reversed list``() =
        let result = reverse [1; 2; 2; 3]
        Assert.That(result, Is.EqualTo([3; 2; 2; 1]))

[<TestFixture>]
type FindTests() =

    [<Test>]
    member this.``Find in empty list should return None``() =
        let result = find 1 []
        Assert.That(result, Is.EqualTo(None))

    [<Test>]
    member this.``Find in single-element list should return 0``() =
        let result = find 1 [1]
        Assert.That(result, Is.EqualTo(Some 0))

    [<Test>]
    member this.``Find in multiple-element list should return index of element``() =
        let result = find 3 [1; 2; 3; 4]
        Assert.That(result, Is.EqualTo(Some 2))

    [<Test>]
    member this.``Find in list with duplicate elements should return index of first occurance``() =
        let result = find 2 [1; 2; 2; 3]
        Assert.That(result, Is.EqualTo(Some 1))

    [<Test>]
    member this.``Find in list without element should return None``() =
        let result = find 4 [1; 2; 2; 3]
        Assert.That(result, Is.EqualTo(None))

[<TestFixture>]
type getListOfpowersOfTwoTests() =
    [<Test>]
    member this.``Get list with m < n should return empty list``() =
        let result = getListOfpowersOfTwo 5 1
        Assert.That(result, Is.EqualTo([] : double list))

    [<Test>]
    member this.``Get list with n <= m and n, m > 0 should return correct list``() =
        let result = getListOfpowersOfTwo 1 5
        Assert.That(result, Is.EqualTo([2.0; 4.0; 8.0; 16.0; 32.0]))

    [<Test>]
    member this.``Get list with n <= m and n, m < 0 should return correct list``() =
        let result = getListOfpowersOfTwo -3 -1
        Assert.That(result, Is.EqualTo([0.125; 0.25; 0.5]))
    
    [<Test>]
    member this.``Get list with n <= m and n < 0 and m > 0 should return correct list``() =
        let result = getListOfpowersOfTwo -3 3
        Assert.That(result, Is.EqualTo([0.125; 0.25; 0.5; 1.0; 2.0; 4.0; 8.0]))