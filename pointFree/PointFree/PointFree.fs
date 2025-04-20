/// A module containing original and point-free function.
module PointFree

/// The original function.
let originalFunction x l = List.map (fun y -> y * x) l

/// A transformed function.
let transformedFunction1 x = List.map (fun y -> y * x)

/// A more transformed function.
let transformedFunction2 x = List.map((*) x)

/// A point-free function.
let transformedFunction3 = List.map << (*)