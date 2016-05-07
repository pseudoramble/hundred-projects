#nowarn "40"

(* Generator based on the following formula:
   http://www.northeastern.edu/afeiguin/phys5870/phys5870/node58.html *)
let createGenerator seed a c m =
    let rec generator =
        seq {
            yield! Seq.scan (fun cur _ -> (a * cur + c) % m) seed generator
            }
        
    generator

(* Another attempt at the generator above. This is to try and break down what's happening above a bit more.
   The gist of this is that we initialize the seq with `seed` and delay the subsequent steps until later.
   An initial real value is important because of laziness. Without it, we force evaluation to occur immediately!

   Here's some helpful commentary:
   https://github.com/fsharp/fsharp/blob/37a100b7caafde0f4df5a1924c9f65f4a18277a8/src/fsharp/FSharp.Core/seq.fs#L488
*)
let buildGenerator seed a c m =
    let foldfn cur =
        (a * cur + c) % m

    let rec generator =
        Seq.append [seed] (Seq.delay (fun () -> Seq.map foldfn generator))

    generator
    
let a, c, m, seed = (27184849, 27184867, 27184879, 27184903)
let rng = (*buildGenerator seed a c m*) createGenerator seed a c m
printfn "%A" rng
printfn "%d" <| Seq.head (Seq.tail rng)
printfn "%d" <| Seq.head (Seq.tail (Seq.tail rng))

let firstPeriod = Seq.take 10 rng
for i in firstPeriod do
    printfn "%d" i

