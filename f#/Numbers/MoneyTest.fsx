#load "Money.fs"

open FSharp.Numerics

let a = Money 10.2
let b = Money 10.1
let pi = Money 3.14159
let roundUp = Money 2.456

printfn "Here are some simple tests to check that the Money type seems to work"
printfn "----------------------------------------------------------------------"

printfn "Two Bucks: %s" <| string (Money 2.0)
printfn "$4.55 - $4.49 = %s" <| string ((Money 4.55) - (Money 4.49)) // $0.06 (no rounding)
printfn "$4.55 + $4.49 = %s" <| string ((Money 4.55) + (Money 4.49)) // $9.04 (no rounding)
printfn "$3.14159 + 2.456 = %s" <| string (pi + roundUp)             // $3.14 + $2.46 = $5.6 (no rounding + rounding)
printfn "$5.00 * 0.5 = %s" <| string ((Money 5.0) * 0.5)             // $2.50
printfn "$5.00 / 2.0 = %s" <| string ((Money 5.0) / 2.0)             // $2.50
printfn "$3.00 / 2.0 = %s" <| string ((Money 3.0) / 2.0)             // $1.50
printfn "%s > %s? %s" (string a) (string b) (string (a > b))         // True
printfn "%s > %s? %s" (string a) (string a) (string (a > a))         // False
printfn "%s > %s? %s" (string b) (string a) (string (b > a))         // False
printfn "%s >= %s? %s" (string a) (string b) (string (a >= b))       // True
printfn "%s >= %s? %s" (string a) (string a) (string (a >= a))       // True
printfn "%s >= %s? %s" (string b) (string a) (string (b >= a))       // False
printfn "%s = %s? %s" (string b) (string b) (string (b = b))         // True
printfn "%s = %s? %s" (string b) (string a) (string (b = a))         // False

