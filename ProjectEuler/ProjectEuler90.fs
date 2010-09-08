module ProjectEuler90

open System.Collections.Generic

//Problem 95
let projecteuler95_soln1 = 
    let cache = Dictionary<int, int>()
    let sumdivisors n = 
        let max = int(System.Math.Ceiling(System.Math.Sqrt(double(n))))
        seq { 2 .. max } |> (Seq.fold (fun acc e -> if n % e = 0 then e::(n/e)::acc else acc) [1]) |> List.sum
    let getamichain n =
        if not(cache.ContainsKey(n)) then
            let amiseq = (Seq.unfold (fun state -> if (sumdivisors state) = n || (sumdivisors state) = 1 || (state > 1000000) then None else Some (sumdivisors state, sumdivisors state)) n) |> Seq.append { n .. n }
            let seqlen = (Seq.length amiseq)
            amiseq |> Seq.iter ( fun e -> cache.[e] <- 0)
            cache.[n] <- 0
            if (amiseq |> Seq.max) < 1000000 || (amiseq |> Seq.min < 2) then
                amiseq |> Seq.iter (fun e -> cache.[e] <- seqlen)
                cache.[n] <- seqlen - 1
        cache.[n]
    seq { 100 .. 200 } |> Seq.map (fun e -> if cache.ContainsKey(e) then cache.[e] else (getamichain e)) |> Seq.max;;
    


//Problem 97
let projecteuler97_soln1 = 
    let nthpowermod n m t = 
        seq { 1 .. m } |> Seq.fold ( fun acc e -> (n*acc % t)) 1
    (28433 * (nthpowermod 2 7830457 1000000000))%1000000000 + 1;;

