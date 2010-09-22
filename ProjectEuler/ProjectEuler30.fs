module ProjectEuler30

open System
open System.Collections.Generic

//Problem 30
let projecteuler30_soln1 = 
    let sumoffifthpower n = n.ToString() |> Seq.fold (fun acc e -> acc + System.Math.Pow(Double.Parse(e.ToString()), 5.0)) 0.0
    seq { 1 .. 1000000 } |> Seq.fold (fun acc e -> if (int(sumoffifthpower e)) = e then acc + e else acc) 0

//Problem 34
let projecteuler34_soln1 =
    let sumFactDigit n = n.ToString() |> Seq.fold (fun acc e -> Int32.Parse(e.ToString()) + acc) 0
    seq { 1 .. 100000 } |> Seq.fold (fun acc e -> if (sumFactDigit e) = e then acc + e else acc) 0*)
    
//Problem 35 (Make this much faster, many computations can be avoided, for example, is using the forall method necessary?)
let projectEuler35_soln1 =
    let cache = Dictionary<bigint, bool>()
    let isCircularPrime l =
        if cache.ContainsKey(l) then cache.[l] else
            let laslist = l.ToString() |> Seq.map (fun e -> Int32.Parse(e.ToString())) |> Seq.toList
            let rotations = Utils.listRotations laslist (List.length laslist - 1) |> List.map (fun e -> Utils.listtoint e 0I)
            rotations |> List.forall (fun e -> (if (not (cache.ContainsKey(e))) then cache.[e] <- (Utils.isprime e);
                                                Console.WriteLine(e.ToString() + " " + cache.[e].ToString()); cache.[e]))
    seq { 2I .. 1000000I } |> Seq.fold (fun acc e -> if isCircularPrime e then acc + 1I else acc) 0I

//Problem 36
let projecteuler36_soln1 =
    seq { 1 .. 1000000 } |> Seq.fold (fun acc n -> if (Utils.ispalindrome (n.ToString() |> Seq.toList) && Utils.ispalindrome (Utils.convtobin n)) then acc + n else acc) 0

//Problem 39
