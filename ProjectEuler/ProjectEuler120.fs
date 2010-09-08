module ProjectEuler120

open System 
open System.Collections.Generic

//Problem 120
let projecteuler120_soln1 =
    { 3 .. 1000 } |> Seq.map ( fun e -> seq { 1 .. 1000 } |> Seq.map (fun d -> (2*d*e) % (e*e)) |> Seq.max) |> Seq.sum

//Problem 122 (TODO: Gives incorrect output, some additions need not be done)
let projecteuler122_soln1 = 
    let cache = Dictionary<int,int>()
    cache.[1] <- 1
    cache.[2] <- 1
    cache.[3] <- 2
    let findMinExp n =
        [ for i in 1 .. n-1 do
            yield (n-i,n)] |> (List.fold (fun acc (a,b) -> if (cache.[a] + cache.[b]) < acc then cache.[a] + cache.[b] else acc)) 99999
    [ 4 .. 15] |> List.map( fun e -> findMinExp e)

//Problem 123
let projecteuler123_soln1 =
    seq { 71069I .. 1000000I } |> Seq.mapi ( fun i e -> if (Utils.isprime e) && ((2I * (bigint(i) + 7036I) * e) >10000000000I) then e else 0I) |> Seq.filter ( fun e -> e > 0I) |> Seq.head

