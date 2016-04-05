open System

type PaymentEntry = {
    toInterest : float;
    toPrinciple : float;
    principleLeft : float;
    month : int;
    };

(* Variables in calculation;
 *  P - Principle (like 100k loan for the house)
 *  r - Monthly interest rate (divide annual by 12 months)
 *  n - Total payments being made (months the loan will last)
 *
 *         r(1 + r)^n
 * M = P --------------
 *        (1 + r)^n - 1
 *)
let monthlyPayments p r n =
    let rFactor = (1.0 + r) ** float(n)
    Math.Round(p * ((r * rFactor) / (rFactor - 1.0)), 2)

let step (p:float) r m i =
    let toInterest = p * r
    let toPrinciple = m - toInterest
    let newPrinciple = p - toPrinciple

    {
        toInterest = toInterest;
        toPrinciple = min toPrinciple (newPrinciple + toPrinciple); (* newPrinciple < 0 => last payment. Remove leftover! *)
        principleLeft = max (Math.Round(newPrinciple, 2)) 0.0; (* Either we have payments left or we're done! *)
        month = i;
        }

let schedule p r m = 
    let rec scheduleAux q sched i =
        let result = step q r m i
        
        if q > 0.0
        then scheduleAux result.principleLeft (result :: sched) (i + 1)
        else sched

    scheduleAux p List.empty 1
    |> List.rev

let toPaymentEntryStr { month = m; toInterest = ti; toPrinciple = tp; principleLeft = pl } =
    sprintf "%5d | $%-7.2f | $%-8.2f | $%.2f" m ti tp pl

let printOut paymentSchedule =
    let header = sprintf "%s | %s | %s | %s" "Month" "Interest" "Principle" "Remaining"

    paymentSchedule
    |> List.map toPaymentEntryStr
    |> String.concat "\n"
    |> (fun res -> header + "\n" + res)

let p = 100000.0
let r = 0.005
let n = 180

let monthly = monthlyPayments p r n
let paymentSchedule = schedule p r monthly

printfn "%s" <| printOut paymentSchedule
