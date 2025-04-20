module Tests

open FsCheck.Xunit
open PointFree

[<Property>]
let ``All functions are equivalent`` (x : int) (l : int list) =
    let result1 = originalFunction x l
    let result2 = transformedFunction1 x l
    let result3 = transformedFunction2 x l
    let result4 = transformedFunction3 x l
    
    result1 = result2 && result2 = result3 && result3 = result4