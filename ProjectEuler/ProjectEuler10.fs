module ProjectEuler2
open System.Collections.Generic
//Problem 11

//Problem 12: In progress
let projecteuler12 =
    let cache = Dictionary<bigint, int>()
    cache.[2I] <- 2
    cache.[3I] <- 2
    let findfactors (n:bigint) = 
        if n % 2I = 0I then
            if n/2I % 2I = 0I then
                cache.[n] <- 2*(cache.[n/2I]-2) 
            else
                cache.[n] <- cache.[n/2I] + 1 
        else
            let max = bigint(System.Math.Ceiling(System.Math.Sqrt(float(n))))
            cache.[n] <- Seq.fold ( fun acc (e:bigint) -> if n % e = 0I then acc + 2 else acc ) 0 (seq { 2I .. max } |> Seq.sortBy (fun x -> -x))
        cache.[n]
    findfactors 28I
        

//Problem 13

//Problem 14 - Uses memoization (refer to Don Syme's blog entry : http link)
//TODO Clean up
let projecteuler14 =
    let cache = Dictionary<bigint,bigint>()
    let getcollatzchain n =
        let rec getcollength n (a:bigint) length = 
            if cache.ContainsKey(a) then
                cache.[n] <- (cache.[a] + length)
                //printfn "%d %d" n cache.[n]
                length + cache.[a]
            else
                if a = bigint(1) then 
                    cache.[n] <- length
                    //printfn "%d %d" n cache.[n]
                    length
                else
                    if a % bigint(2) = bigint(0) then
                        getcollength n (a/bigint(2)) (length+bigint(1))
                    else
                        getcollength n (bigint(3)*(a)+bigint(1)) (length+bigint(1))
        getcollength n n (bigint(0))
    Seq.fold ( fun acc e -> if (getcollatzchain e) > acc then e else acc) (bigint(1)) (seq { bigint(2) .. bigint(100) });;
    
//Problem 15
let projecteuler15_soln1 = 
    let cache = Dictionary<int, Dictionary<int, int>>()
    let gridcoords =
        [ for i in 1 .. 21 do
            for j in 1 .. 21 do
                yield (i,j)] |> List.map (fun e -> match e with
                                                    |(a,b) -> if a = 1 then 
                                                                cache.[b] <- Dictionary<int, int>()
                                                                cache.[b].[a] <- 1
                                                              elif b = 1 then
                                                                cache.[b].[a] <- 1
                                                              else
                                                                cache.[b].[a] <- cache.[b-1].[a] + cache.[b].[a-1])
    cache.[21].[21];;

//Problem 16
let projecteuler16_soln1 =                                                 
    let multlist n =
        let rec findlist n acc =
            match n with
            |h::t -> if (h*2 + acc) < 10 then 
                        ((h*2 + acc)%10)::(findlist t 0)
                     else
                        ((h*2 + acc)%10)::(findlist t 1) 
            |[] -> if acc = 1 then [1] else []
        (findlist (n |> List.rev) 0) |> List.rev
    seq { 1 .. 10000} |> (Seq.fold ( fun acc e -> multlist acc) [1]) |> Seq.sum

let projecteuler16_soln2 = 
    (2I ** 1000).ToString().ToCharArray() |> Seq.fold ( fun acc e -> acc + System.Int32.Parse(e.ToString())) 0;; 

//Problem 17
//Fill up cache with base values 1 2 3 4...19 and the tens and hundreds, also we are counting spaces
let projecteuler17_soln1 =
    let cache = Dictionary<int, string>()
    let rec convnumtoword n m =
        if cache.ContainsKey(n) then cache.[n] else
            if m = 100 && m < n then
                cache.[(n / m) * m] + " and " + convnumtoword (n % m) (m/10)
            else
                cache.[(n / 10) * 10] + " " + convnumtoword (n % 10) (m/10)
    ([1 .. 999] |> List.map (fun e -> (convnumtoword e 100) |> Seq.length) |> List.sum) + 12

//Problem 18
//This is a trivial DP problem that i have no interest in doing (ie left as an exercise for the gentle reader)

//Problem 19

//Problem 20
