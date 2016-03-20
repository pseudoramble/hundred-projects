let rec fib n =
    match n with
        | 2 | 1 -> 1
        | _ -> fib (n - 1) + fib (n - 2)
    
let input = 10
let fibbers = seq { for i in 1 .. input -> (i, fib i) }
for (n, fibber) in fibbers do
    printfn "fib %d = %d" n fibber
