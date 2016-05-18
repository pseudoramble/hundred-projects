#r "packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll"
#load "RNG.fs"

open RNG
open FSharpx.Collections

let a, c, m, seed = (1103515245, 12345, Core.int.MaxValue, Core.int.MaxValue)
let rng = RNG.createLazyListGenerator seed a c m

RNG.next rng None
|> fst
|> printfn "Next (no bounds!) %d"

let maxTest = Some (RNG.Max 20)
let minTest = Some (RNG.Min (Core.int.MaxValue - 1))

RNG.next rng maxTest
|> fst
|> printfn "Next (Max = %A) %d" maxTest

RNG.next rng minTest
|> fst
|> printfn "Next (Min = %A) %d" minTest

let n = 50000
RNG.take rng None (Some n)
|> fst
|> LazyList.rev
|> printfn "Take %d! %A" n

let p = 50
RNG.take rng minTest (Some p)
|> fst
|> LazyList.rev
|> printfn "Take %d! Min = %A. %A" p minTest
