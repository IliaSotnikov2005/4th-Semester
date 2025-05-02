open LambdaInterpretator

let example = "
x y
"

printf "%s\n" ((interpretFromFile "example.txt").PrettyPrint ())