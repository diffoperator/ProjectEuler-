module ProjectEuler60

open System.Collections.Generic

//TODO: Problem 63 (gives 40 while answer is supposed to be 49.)
let projecteuler63_soln1 = 
    [for i in 4 .. 9 do
        for j in 2 .. 100000000 do
            yield (i,j) ] |> List.filter ( fun (i,j) -> Utils.numdigit(bigint(i) ** j) = j ) |> Seq.length;;

//Problem 62
let projecteuler62_soln1 =
    let cache = Dictionary<char list, bigint list>()
    let poweroffive_list (n:bigint) =
        (n ** 3).ToString() |> Seq.toList |> List.sort
    [1I .. 10000I] |> List.map (fun e -> if cache.ContainsKey(poweroffive_list e) then cache.[poweroffive_list e] <- cache.[poweroffive_list e] |> List.append [e]  else cache.[poweroffive_list e] <- [e]) |> ignore
    ((cache.Values |> Seq.filter (fun e -> e.Length > 4) |> Seq.head) |> List.min) ** 3
