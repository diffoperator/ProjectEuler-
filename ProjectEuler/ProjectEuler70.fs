module ProjectEuler70

open System.Collections.Generic

//Problem 72
//This solution is very slow, even with the memoization it takes 15 seconds for n = 1000, will take about 5 hours for n = 1 mil
let projecteuler72_soln1 =
    let findreducedfractions n =
        let cache = Dictionary<int * int, int>()
        let rec gcd a b =
            match b with
            | 0 -> a
            | _ when cache.ContainsKey((a,b)) -> cache.[(a,b)]
            | _ -> cache.[(a,b)] <- gcd b (a % b); cache.[(a,b)]
        [for i in 2 .. n do
            for j in (i + 1) .. n do
                if (gcd i j) = 1 then yield (i,j)]
    findreducedfractions 1000000
//Another "cleverer" solution is to simply sum the euler totient of the numbers from 2 to 1 million, but this one is slow as hell too
let projecteuler72_soln2 =
    [2I .. 1000000I] |> List.map (fun e -> Utils.eulertotient e) |> List.sum

//Actually we need a way of generating all coprime numbers for a given n

//Problem 76
//let projecteuler76_soln1 =
    