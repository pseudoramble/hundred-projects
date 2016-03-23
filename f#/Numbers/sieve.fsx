let primes n =
    let step current toRemove =
        current
        |> (fun nums -> Set.difference (Set.ofArray nums) toRemove)
        |> Set.toArray

    let natNumbers = Array.ofSeq (seq { for i in 2 .. n -> i })
    let mutable results = natNumbers
    
    for i in 0 .. int (sqrt (float n)) do
        if Array.length results > i
        then
            let nextNumber = Array.get results i
            results <-
                    Set.ofSeq (seq { for j in nextNumber .. n -> nextNumber * j })
                    |> fun nextSet -> step results nextSet

    results

let lePrimes = primes 100
printf "%A \n(length = %d)" <| lePrimes <| Array.length lePrimes
