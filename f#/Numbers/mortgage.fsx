open System

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

printfn "$%.2f per month" <| monthlyPayments 100000.0 0.005 180
