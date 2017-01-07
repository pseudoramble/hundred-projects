let primes n =
    let generateStep multiplier limit removals =
        if not (Set.contains multiplier removals)
        then
            seq { for j in (pown multiplier 2) .. multiplier .. limit -> j }
            |> Set.ofSeq
            |> Set.union removals
        else removals

    let rec runTest multipliers limit removals =
        let multipler = List.head multipliers

        if multipler > int (sqrt (float limit))
        then
            removals
        else
            let updatedRemovals = generateStep multipler limit removals
            runTest (List.tail multipliers) limit updatedRemovals

    match n with
        | 1 -> Set.empty
        | _ ->
            let startSeq = List.ofSeq (seq { for i in 2 .. n -> i })
            
            startSeq
            |> fun lst -> runTest lst n Set.empty
            |> Set.difference (Set.ofSeq startSeq)

#if INTERACTIVE
let limit = 250
#else
let limit = Some (int32 (System.Console.ReadLine()))
#endif

let lePrimes = primes limit
printfn "%A" <| lePrimes
Seq.rev lePrimes
|> Seq.collect (fun x -> Seq.singleton (string x))
|> Seq.reduce (fun result next -> sprintf "%s, %s" result next)
|> printfn "[%s]"

// printfn "(length = %d)" <| Seq.length lePrimes
// printfn "(last prime = %d)" <| Seq.last lePrimes
