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
                    Set.ofSeq (seq { for j in (pown nextNumber 2) .. nextNumber .. n -> j })
                    |> fun nextSet -> step results nextSet

    Seq.ofArray results

let lePrimes = primes 1000000
printfn "%A" lePrimes
printfn "(length = %d)" <| Seq.length lePrimes
printfn "(last prime = %d)" <| Seq.last lePrimes
