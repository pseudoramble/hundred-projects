#load "Money.fs"
open System
open FSharp.Numerics

type PaymentEntry = {
    toInterest : Money;
    toPrinciple : Money;
    principleLeft : Money;
    month : int;
    };

let monthlyPayments (p:Money) r n =
    let rFactor = (1.0 + r) ** float(n)
    p * ((r * rFactor) / (rFactor - 1.0))

let step (p:Money) r m i =
    let toInterest = p * r
    let toPrinciple = m - toInterest
    let newPrinciple = p - toPrinciple

    {
        toInterest = toInterest;
        toPrinciple = min toPrinciple (newPrinciple + toPrinciple); (* newPrinciple < 0 => last payment. Remove leftover! *)
        principleLeft = max newPrinciple (Money 0.0); (* Either we have payments left or we're done! *)
        month = i;
        }

let schedule p r m = 
    let rec scheduleAux q sched i =
        let result = step q r m i
        
        if q > Money 0.0
        then scheduleAux result.principleLeft (result :: sched) (i + 1)
        else sched

    scheduleAux p List.empty 1
    |> List.rev

let toPaymentEntryStr { month = m; toInterest = ti; toPrinciple = tp; principleLeft = pl } =
    sprintf "%5d | %-8s | %-9s | %s" m (string ti) (string tp) (string pl)

let printOut paymentSchedule =
    let header = sprintf "%s | %s | %s | %s" "Month" "Interest" "Principle" "Remaining"

    paymentSchedule
    |> List.map toPaymentEntryStr
    |> String.concat "\n"
    |> (fun res -> header + "\n" + res)

let p = Money 100000.0
let r = 0.005
let n = 180

let monthly = monthlyPayments p r n
let paymentSchedule = schedule p r monthly

printfn "%s" <| printOut paymentSchedule
