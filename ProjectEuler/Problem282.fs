module Problem282

open System.Collections.Generic

let projecteuler282_soln1 =
    let cache = Dictionary<int, Dictionary<int, bigint>>()
    let rec getackermann m n =
        if not (cache.ContainsKey(m)) then
            cache.[m] <- Dictionary<int, bigint>()    
        if cache.[m].ContainsKey(n) then
            cache.[m].[n]
        else       
            if m = 0 then
                cache.[m].[n] <- bigint n + 1I
                bigint n + 1I
            elif m > 0 && n = 0 then
                cache.[m].[n] <- getackermann (m - 1) 1
                cache.[m].[n]
            else
                cache.[m].[n] <- getackermann (m - 1) (int (getackermann m (n - 1)))
                cache.[m].[n]
    printfn "%d" cache.[0].Count
    getackermann 1 0let projecteuler282_soln1 =
    let cache = Dictionary<int, Dictionary<int, bigint>>()
    let rec getackermann m n =
        if not (cache.ContainsKey(m)) then
            cache.[m] <- Dictionary<int, bigint>()    
        if cache.[m].ContainsKey(n) then
            printfn "Value used %d %d" m n
            cache.[m].[n]
        else       
            if m = 0 then
                cache.[m].[n] <- bigint n + 1I
                bigint n + 1I
            elif m = 1 then
                cache.[m].[n] <- bigint n + 2I
                bigint n + 2I
            elif m = 2 then
                cache.[m].[m] <- cache.[m].[n-1] + 2I
                cache.[m].[n-1] + 2I
            elif m > 0 && n = 0 then
                cache.[m].[n] <- getackermann (m - 1) 1
                cache.[m].[n]
            else
                cache.[m].[n] <- getackermann (m - 1) (int (getackermann m (n - 1)))
                cache.[m].[n]
    getackermann 3 3;;

let ackermann m n =
    let cache = Dictionary<int, Dictionary<int, int>>()
    [ for i in 0 .. m do
        for j in 0 .. n do
            yield (i,j) ] |> List.map (fun (a,b) -> if not (cache.ContainsKey(a)) then
                                                        cache.[a] <- Dictionary<int,int>()
                                                    if a = 0 then 
                                                        cache.[a].[b] <- b+1
                                                    elif a > 0 && b = 0 then
                                                        cache.[a].[b] <- cache.[a-1].[1]
                                                    else
                                                        cache.[a].[b] <- cache.[a-1,cache.[a,b-1]])