module ProjectEuler100onwards

//Problem 108 
//no good for 100+ need something faster
let projecteuler108_soln1 = 
    let findsoln n = 
        seq { (n+1) .. 2*n } |> Seq.filter ( fun e -> n * e % (e - n) = 0 )
    Seq.unfold ( fun state -> if ((findsoln state) |> Seq.length > 100) then None else Some (state, state + 1)) 2 |> Seq.max

