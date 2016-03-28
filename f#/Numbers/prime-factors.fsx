(*
   Prime Factorization requires two main parts - Generating prime numbers and the actual test.
   This will implement the Sieve of Eratosthenes (https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) for prime generation.
   This will also implement trial division (https://en.wikipedia.org/wiki/Trial_division) to do the actual testing. *)
#load "sieve.fsx"

let primeFactors n =
    let rec path a b steps tests =
        if a % b = 0
        then path (a/b) b (b :: steps) tests
        else
            match tests with
                | x :: xs -> if x * x > a then a :: steps else path a x steps xs
                | [] -> a :: steps

    let primes =
        Sieve.primes (int (sqrt (float n)))
        |> Set.toList

    if List.length primes > 0
    then
        match path n (List.head primes) [] (List.tail primes) with
            | [x] -> [1;x]
            | res -> List.filter (fun x -> x <> 1) res
    else
        [1;n]

let knownPrimeFactors =
    Map.empty
    |> Map.add 2 [1;2]   |> Map.add 3 [1;3]    |> Map.add 4 [2;2]
    |> Map.add 5 [1;5]   |> Map.add 6 [3;2]    |> Map.add 7 [1;7]
    |> Map.add 8 [2;2;2] |> Map.add 9 [3;3]    |> Map.add 10 [5;2]
    |> Map.add 11 [1;11] |> Map.add 12 [3;2;2] |> Map.add 13 [1;13]
    |> Map.add 14 [7;2]  |> Map.add 15 [5;3]   |> Map.add 16 [2;2;2;2]
    
for i in 2 .. 16 do
    if (Map.find i knownPrimeFactors) = primeFactors i
    then printfn "Prime factors of %d = %A" i (primeFactors i)
    else printfn "You done messed up on %d (expected = %A, actual = %A)" i <| Map.find i knownPrimeFactors <| primeFactors i

let i = 2147483600
printfn "Prime factors of %d = %A" i (primeFactors i)
