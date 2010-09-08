module ProjectEuler1

open System.Collections.Generic;

//Problem 1
let projecteuler1_soln1 = List.sum [ for i in 1 .. 1000 do if i % 3 = 0 || i % 5 = 0 then yield i ]

let projecteuler1_soln2 = seq { 1 .. 999 } 
                          |> Seq.filter (fun i -> i%5 = 0 || i%3 = 0) 
                          |> Seq.sum

//Problem 2
let projecteuler2_soln1 = Seq.fold( fun acc e -> if e % 2 = 0 then acc + e else acc) 0 (Seq.unfold (fun state ->
    if (snd state > 4000000) then None
    else Some(fst state + snd state, (snd state, fst state + snd state))) (1,1))

//Problem 3
let projecteuler3_soln1 =
    List.tail (Utils.primefactors 600851475143.0)

//Problem 4
let projecteuler4_soln1 = seq { for i in 100 .. 999 do
                                    for j in 100 .. 999 do
                                        yield (i,j) 
                                        } |> Seq.filter ( fun (x,y) -> Utils.ispalindrome ((x*y).ToString() |> Seq.toList) )

//Problem 5

//Problem 6
let projecteuler6_soln1 = Seq.sum (seq { for i in 1 .. 100 do
                                            for j in 1 .. 100 do
                                                yield 2*i*j })

//Problem 7

//Problem 8

//Problem 9
let projecteuler9_soln1 = Seq.head (seq { for i in 1 .. 500 do
                                            for j in i .. 500 do
                                                if j*j + j*i = 500 then
                                                    yield (j*j - i*i, 2*j*i, j*j+i*i)})