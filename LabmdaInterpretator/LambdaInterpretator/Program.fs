open LambdaInterpretator

let skk = "
let true = \\a.\\b.a
let false = \\a.\\b.b
let if = \\p.\\t.\\e.p t e

let isZero = \\n.n (\\c . false) true
let pair = \\a.\\b.\\t.t a b
let fst = \\p.p true
let snd = \\p.p false
let pred = \\n.\\s.\\z.snd (n (\\p.pair(s (fst p)) (fst p)) (pair z z))
let mult = \\n.\\m.\\s.\\z.n (m s) z

let fix = \\f.(\\x.f (x x)) (\\x.f (x x))

let facty = \\f.\\x.if (isZero x) one (mult x (f (pred x)))

let zero = \\f.\\x.x
let one = \\f.\\x.f x
let two = \\f.\\x.f (f x)
let three = \\f.\\x .f (f (f x))

let fact = fix facty

fact three"

let result = (interpretString skk).PrettyPrint ()

printf "%s\n" result