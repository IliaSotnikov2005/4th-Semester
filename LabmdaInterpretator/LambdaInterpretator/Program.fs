open LambdaInterpretator

let example = "
x y
"

printf "%s\n" ((interpretString "\x.y (a b)").PrettyPrint ())