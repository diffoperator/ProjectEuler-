module ProjectEuler20

open System.Collections.Generic

//Problem 20
let projecteuler20_soln1 = (seq { 2I .. 100I } |> Seq.fold ( fun acc e -> (e * acc)) 1I).ToString() 

//Problem 21 (remember to remove integers whose sum of divisors equal themselves)
let projecteuler21_soln1 = 
    let sumdivisors n = 
        let max = int(System.Math.Ceiling(System.Math.Sqrt(float(n))))
        seq { 2 .. max } |> (Seq.fold (fun acc e -> if n % e = 0 then e::(n/e)::acc else acc) [1]) |> List.sum
    seq { 2 .. 10000 } |> (Seq.fold (fun acc e -> if sumdivisors(sumdivisors e) = e && not(sumdivisors e = e) then e::acc else acc) []) |> List.sum

//Probem 24 (permutation code taken from F# for scientists)
let projecteuler23_soln1 =
    let rec distribute e = function
      | [] -> [[e]]
      | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    let rec permute = function
      | [] -> [[]]
      | e::xs -> List.collect (distribute e) (permute xs)
    permute [0;1;2;3;4;5;6;7;8;9] |> (Seq.nth 2628799) |> Seq.toList |> List.rev;;

//Problem 25 (bigint is crazy)
let projecteuler25_soln1 = 
   (Seq.unfold (fun state ->
    if (snd state > 4000000) then None
    else Some(fst state + snd state, (snd state, fst state + snd state))) (1,1)) |> Seq.filter ( fun x -> (x.ToString() |> Seq.length) > 1000) |> Seq.head

//Problem 27 (TODO print the actual answer instead of the number of primes)
let projecteuler27_soln1 =
    let generateNumbers (a:int) (b:int) =
        (Seq.unfold (fun state -> if Utils.isprime(state*state + a*state + b) then Some(state,state+1) else None) 0 |> Seq.length)
    [ for i in -70 .. 70 do
        for j in i*i/4 .. 1000 do
            if i <> 0 && j <> 0 then yield (i,j) ] |> List.map ( fun (i,j) -> generateNumbers i j) |> List.max

//Problem 28
let projecteuler28_soln1 = 
    1I + ([for i in 3I .. 1001I do
            if i%2I <> 0I then
                yield [i*i; i*i-i+1I; i*i-2I*i+2I; i*i-3I*i+3I] |> List.sum] |> List.sum)

//Problem 29
let projecteuler29_soln1 =
    let rec factors (n:bigint) (e:bigint) =
        if n = 1I then [] else if n % e = 0I then (e)::(factors (n/e) e) else (factors n (e+1I))
    let rec factorsxupy (x:bigint) (y:bigint) =
        if y = 0I then
            []
        else
            List.append (factors x 2I) (factorsxupy x (y-1I))
    let cache = Dictionary<bigint list, int>()
    [ for i in 2I .. 100I do
        for j in 2 .. 100 do
            yield (i,j) ] |> List.fold ( fun acc e -> if not(cache.ContainsKey(factorsxupy (fst e) (snd e))) then cache.[factorsxupy (fst e) (snd e)] <- 1 
                                                                                                                  (fst e,snd e)::acc else acc) [(1I,1)] |> ignore 
    cache.Count;;