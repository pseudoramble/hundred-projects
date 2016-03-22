let pi n =
    let calculate n =
        let fn = float(2 * n)

        if n % 2 = 0
        then -4.0 / (fn * (fn + 1.0) * (fn + 2.0))
        else 4.0 / (fn * (fn + 1.0) * (fn + 2.0))

    let terms = seq { for i in 1 .. n -> calculate i }
    let total = Seq.reduce (+) terms
    3.0 + total
    
let input = 1214
printfn "pi ~= %.10f" <| pi input
