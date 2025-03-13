module PointFree

let originalFunction x l = List.map (fun y -> y * x) l

let transformedFunction1 x = List.map (fun y -> y * x)
let transformedFunction2 x = List.map((*) x)
let transformedFunction3 = List.map << (*)