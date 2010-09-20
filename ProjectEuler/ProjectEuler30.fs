module ProjectEuler30

open System

//Problem 30
let projecteuler30_soln1 = 
    let sumoffifthpower n = n.ToString() |> Seq.fold (fun acc e -> acc + System.Math.Pow(Double.Parse(e.ToString()), 5.0)) 0.0
    seq { 1 .. 1000000 } |> Seq.fold (fun acc e -> if (int(sumoffifthpower e)) = e then acc + e else acc) 0

//Problem 39
