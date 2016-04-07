namespace FSharp.Numerics
open System

type Money =
    | Money of float
    member x.ToFloat() =
        let (Money y) = x in y
    override x.ToString() =
        sprintf "$%.2f" <| x.ToFloat()

    static member Create(m:float) =
        Money (Math.Round(m, 2))

    static member (+) (Money a, Money b) = Money.Create(Math.Round(a, 2) + Math.Round(b, 2))
    static member (-) (Money a, Money b) = Money.Create(Math.Round(a, 2) - Math.Round(b, 2))
    static member (*) (Money a, b:float) = Money.Create(Math.Round(a, 2) * b)
    static member (/) (Money a, b:float) = Money.Create(Math.Round(a, 2) / b)
    static member op_GreaterThan (Money a, Money b) = a > b
    static member op_LessThan (Money a, Money b) = a < b
    static member op_GreaterThanOrEqual (Money a, Money b) = a >= b
    static member op_LessThanOrEqual (Money a, Money b) = a <= b
    static member op_Equal (Money a, Money b) = a = b
