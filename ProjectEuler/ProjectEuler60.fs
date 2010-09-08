module ProjectEuler60

//TODO: Problem 63 (gives 40 while answer is supposed to be 49.)
let projecteuler63_soln1 = 
    [for i in 4 .. 9 do
        for j in 2 .. 10000 do
            yield (i,j) ] |> List.filter ( fun (i,j) -> Utils.numdigit(bigint(i) ** j) = j ) |> Seq.length;;
