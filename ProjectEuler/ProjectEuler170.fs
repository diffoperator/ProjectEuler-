module ProjectEuler170

//Problem 179
let projecteuler179_soln1 = 
    let numfactors l =
        l |> Seq.fold (fun acc e -> ((snd e) + 1) * acc) 1
    [1 .. 100000] |> List.filter (fun e -> (numfactors (Utils.compress (Utils.findfactors e 2))) = (numfactors (Utils.compress (Utils.findfactors (e+1) 2)))) |> List.length