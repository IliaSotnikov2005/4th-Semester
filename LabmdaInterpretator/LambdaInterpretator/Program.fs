open LambdaInterpretator

let skk = "
let S = \\x y z.x z (y z)
    let K = \\x y.x
S K K"

let result = (interpretString skk).PrettyPrint ()

printf "%s\n" result