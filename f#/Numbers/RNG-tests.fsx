#r "packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll"
#load "RNG.fs"

open RNG
open FSharpx.Collections

let a, c, m, seed = (1103515245, 12345, Core.int.MaxValue, Core.int.MaxValue)
let rng = RNG.createLazyListGenerator seed a c m

(* Some ranges to use in the examples below *)
let maxTest = Some (RNG.Max 20)
let rangeTest = Some (RNG.Range (10, 20))

(* Test getting the next value from the RNG *)
RNG.next rng None
|> fst
|> printfn "Next (no bounds!) %d"

RNG.next rng maxTest
|> fst
|> printfn "Next (Max = %A) %d" maxTest

RNG.next rng rangeTest
|> fst
|> printfn "Next (Max = %A) %d" maxTest

(* Take a big sample from the RNG *)
let n = 500000
RNG.take rng n None
|> fst
|> LazyList.rev
|> (fun lst -> LazyList.length lst = n)
|> printfn "Did we take %d samples? %b" n

(* Take a sample from the RNG while testing some of the range functionality *)
let p = 500
RNG.take rng p maxTest
|> fst
|> LazyList.toSeq
|> Seq.forall (fun x -> x <= 20)
|> printfn "Take %d! All values <= %A? %b" p maxTest

RNG.take rng p rangeTest
|> fst
|> LazyList.toSeq
|> Seq.forall (fun x -> x >= 10 && x <= 20)
|> printfn "Take %d! All values between %A? %b" p rangeTest

let lameRng = RNG.createLazyListGenerator 1024 123 233 312
let lameSample =
    RNG.take lameRng 5 None
    |> fst
    |> LazyList.toSeq
    |> Seq.rev

for i in lameSample do
    printfn "i = %d" i
done
