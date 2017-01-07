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

let printOut paymentSchedule (monthlyRate:Money) n =
    let header = sprintf "Your amortized payment schedule:\n%s | %s | %s | %s" "Month" "Interest" "Principle" "Remaining"

    let printedSchedule =
        paymentSchedule
        |> List.map toPaymentEntryStr
        |> String.concat "\n"
        |> (fun res -> header + "\n" + res)

    let monthlyPayments = sprintf "You will be paying %s a month for %d years." (string monthlyRate) (n / 12)

    sprintf "%s\n\n%s" monthlyPayments printedSchedule

let toMonthlyRate percentageYearly =
    percentageYearly / 12.0 / 100.0

let p = Money 250000.0
let r = toMonthlyRate 3.5(*% APR*)
let n = 360

let monthly = monthlyPayments p r n
let paymentSchedule = schedule p r monthly

printfn "%s" <| printOut paymentSchedule monthly n
