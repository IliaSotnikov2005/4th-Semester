module Tests

open Xunit
open FsUnit.Xunit
open EvenNumbersCounter
open FsCheck.Xunit

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
    ([], 0)
    ([1; 2; 3; 4; 5], 2)
    ([2; 4; 6; 8], 4)
    ([1; 3; 5; 7], 0)
    ([0; -2; -3; -4], 3)
]

[<Theory>]
[<MemberData(nameof(testData))>]
let ``All implementations should count even numbers correctly`` (list: int list, expected: int) =
    for implementation in implementations do
        let result = implementation list
        result |> should equal expected