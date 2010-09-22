module ProjectEuler70

open System.Collections.Generic

//Problem 72
//This solution is very slow, even with the memoization it takes 15 seconds for n = 1000, will take about 5 hours for n = 1 mil
let findreducedfractions n =
    let cache = Dictionary<int * int, int>()
    let rec gcd a b =
        match b with
        | 0 -> a
        | _ when cache.ContainsKey((a,b)) -> cache.[(a,b)]
        | _ -> cache.[(a,b)] <- gcd b (a % b); cache.[(a,b)]
    [for i in 2 .. n do
        for j in (i + 1) .. n do
            if (gcd i j) = 1 then yield (i,j)];;

//Actually we need a way of generating all coprime numbers for a given n

//Problem 76
//let projecteuler76_soln1 =
    