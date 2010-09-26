module ProjectEuler40
open System.Linq
//Problem 41 (remove the 0 in the permute list to get the correct answer, the prime obtained currently could be considered a super pandigital)
let projecteuler41_soln1 =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    permute [0;1;2;3;4] |> Seq.map ( fun e -> Utils.listtoint (List.append [7;6;5] e) 0I) |> Seq.filter ( fun e -> (e%2I <> 0I) && (Utils.isprime e)) |> Seq.toList |> Seq.max;;

//Problem 44
let projecteuler44_soln1 =
    [for i in 1. .. 10000.0 do
        for j in i+1. .. 10000.0 do
            let a = i * (3. * i - 1.)/2.
            let b = j * (3. * j - 1.)/2.
            if Utils.ispentagonal (a + b) && Utils.ispentagonal (b - a) then yield (b-a)]

//Problem 48
let projecteuler48_soln1 =
    let nupn n = 
        Seq.fold (fun (acc:bigint) (e:bigint) -> (acc*e)%10000000000I) 1I (seq { for i in 1 .. n do yield bigint(n) })
    Seq.fold (fun acc e -> (acc + (nupn e))%10000000000I) 0I (seq { 1 .. 1000});;
    