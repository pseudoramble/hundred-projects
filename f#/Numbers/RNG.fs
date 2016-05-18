(* A random number generator... generator, and interface.

   There are two goals here:
   
     1. Poke around at what a RNG does.
     2. Come up with a functional-feeling interface to it.

   The main RNG generator is based on the following formula: http://www.northeastern.edu/afeiguin/phys5870/phys5870/node58.html
   
   There are two attempts at the RNG itself:
     1. A generator based on Seq's.
       * The code itself is quite nice to read.
       * This has the limitation of non-lazy evaluation. New iterators are created constastly for this.
       * Ex: `Seq.tail` become *extremely* slow after a few samples (say around 500 times).
     2. A generator based on LazyList's.
       * The code isn't quite as nice, but not unbearable.
       * This mostly eliminates the major perf issues of the Seq implementation.
       * It requires an external lib to work however.
       
   The gist of #1 is that we initialize the seq with `seed` and delay the subsequent steps until later.
   An initial real value is important because of laziness. Without it, we force evaluation to occur immediately!
   Some helpful explanation and commentary can be found here:
   https://github.com/fsharp/fsharp/blob/37a100b7caafde0f4df5a1924c9f65f4a18277a8/src/fsharp/FSharp.Core/seq.fs#L488
*)

#nowarn "40"

module RNG =
    open FSharpx.Collections

    type Bounds =
        | Min of int
        | Max of int
        | Range of int * int
    
    let createSeqGenerator seed a c m =
        let rec generator =
            seq {
                yield! Seq.scan (fun cur _ -> (a * cur + c) % m) seed generator
                }
            
        generator

    let createLazyListGenerator seed a c m =
        let foldfn cur =
            (a * cur + c) % m
            
        let rec generator =
            LazyList.consDelayed seed (fun () -> LazyList.map foldfn generator)
            
        generator

    let nextMax rng max =
        let rngMax = Core.int.MaxValue
        let rngLimit = rngMax - ((rngMax % max) + 1) % max
        
        let rec nextAux rng =
            let value = LazyList.head rng
            match value > rngLimit with
                | true -> nextAux (LazyList.tail rng)
                | _ -> (value % max, LazyList.tail rng)
        
        nextAux rng

    let nextMin rng min =
        let rngMax = Core.int.MaxValue
        
        match LazyList.head rng with
            | n when n < min -> (n + min, LazyList.tail rng)
            | n -> (n, LazyList.tail rng)

    let nextRange rng min max =
        (0, rng)

    let next rng bounds =
        match bounds with
            | Some (Range (min, max)) -> (LazyList.head rng, LazyList.tail rng)
            | Some (Min min) -> nextMin rng min
            | Some (Max max) -> nextMax rng max
                | None -> (LazyList.head rng, LazyList.tail rng)

    let take rng bounds total =
        let count = defaultArg total 1
        
        let rec takeAux rng curCount results =
            let (nextValue, nextRng) = next rng bounds
            if curCount = 1 then (LazyList.cons nextValue results, nextRng)
            else takeAux nextRng (curCount - 1) (LazyList.cons nextValue results)
        
        takeAux rng count LazyList.empty
