module Tests

open NUnit.Framework
open FsUnit
open EvenNumbersCounter
open FsCheck.NUnit

[<Property>]
let ``All implementations should be equivalent`` (list: int list) =
    let resultMap = mapRealisation list
    let resultFilter = filterRealisation list
    let resultFold = foldRealisation list
    resultMap = resultFilter && resultFilter = resultFold

let implementations = [
    mapRealisation
    filterRealisation
    foldRealisation
]

let testData = [
    TestCaseData( List<int>.Empty, 0).SetName("Empty list should return 0")
    TestCaseData([1; 2; 3; 4; 5], 2).SetName("[1;2;3;4;5] should return 2")
    TestCaseData([2; 4; 6; 8], 4).SetName("[2;4;6;8] should return 4")
    TestCaseData([1; 3; 5; 7], 0).SetName("[1;3;5;7] should return 0")
    TestCaseData([0; -2; -3; -4], 3).SetName("[0;-2;-3;-4] should return 3")
]

[<Test>]
[<TestCaseSource(nameof(testData))>]
let ``All implementations should count even numbers correctly`` (list: int list, expected: int) =
    for implementation in implementations do
        let result = implementation list
        result |> should equal expected