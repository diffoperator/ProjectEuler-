module ProjectEuler5

//Problem 52 (WTF is this? Make it beautiful!)
let projecteuler52_soln1 =
    seq { 1 .. 1000000 } |> Seq.filter (fun e -> ((e*2).ToString() |> Seq.sort |> Seq.toList) = ((e*3).ToString() |> Seq.sort |> Seq.toList) && ((e*4).ToString() |> Seq.sort |> Seq.toList) = ((e*5).ToString() |> Seq.sort |> Seq.toList))

//Problem 53
let projecteuler53_soln1 = 
    let ncr n r =
            (seq { n-r+1I .. n } |> Seq.fold ( fun acc e -> e*acc ) 1I) / (seq { 1I .. r } |> Seq.fold ( fun acc e -> e * acc ) 1I)
    [ for i in 23I .. 100I do
            for j in 2I .. i-1I do
                if (ncr i j) > 1000000I then yield (i,j)] |> List.length //We can remove several iterations (if i,j then i,i-j as well) here but since the solution is fast as it is...meh :|

//Problem 55
let projecteuler55_soln1 = 
    let rec islychrel (e:bigint) iters =
        if iters = 60 || ((Utils.ispalindrome ((Utils.reverseandadd e).ToString().ToCharArray() |> Seq.toList)) && iters = 0) then 
            false
        else
            if (Utils.ispalindrome ((Utils.reverseandadd e).ToString().ToCharArray() |> Seq.toList)) then 
                true
            else
                islychrel (Utils.reverseandadd e) (iters+1)
    [ for i in 10I .. 10000I do yield i ] |> List.filter (fun i -> islychrel i 0) |> List.length;;

//Problem 56
let projecteuler56_soln1 = 
    [for i in 1 .. 100 do
       for j in 1 .. 100 do
            yield (i,j) ] |> List.map ( fun (a,b) -> Utils.SumDigits(bigint(a) ** b)) |> List.max;;

//Problem 57
let projecteuler57_soln1 = 
    let expand n =
        Seq.unfold (fun (num, den) -> Some((num, den), (den*2I + num, den + num))) (3I, 2I) |> Seq.take n
    expand 1000 |> Seq.filter (fun (num, denom) -> num.ToString().Length > denom.ToString().Length) |> Seq.length


//Problem 58 (Is this correct? Probably not. Doesnt give the answer it is supposed to anyway)
let projecteuler58_soln1 = 
    [for i in 3I .. 30001I do
            if i%2I <> 0I then
                for j in 1I .. 3I do
                    if (i*i % (j*i-j) <> 0I) then yield (i*i-j*i+j)] |> List.fold ( fun (primes,total) e -> if ((e%2I<>0I) && (Utils.isprime e)) then (primes+1, total+1) else (primes, total+1)) (0,0);;
