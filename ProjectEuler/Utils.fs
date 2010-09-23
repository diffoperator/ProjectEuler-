module Utils

open System

let sum list =
   let rec loop list acc =
       match list with
       | head :: tail -> loop tail (acc + head)
       | [] -> acc
   loop list 0

let reduceAddNumList acc i = acc + i

let ispalindrome l = 
    if (List.rev l) = l then true else false
    
let trianglenumbergen = Seq.unfold ( fun (a,b) -> if a > 1000 then None else Some(a+b+1,(a+b+1, b+1))) (1,1)

let rec factors (n:bigint) (e:bigint) =
    if n = 1I then [] else if n % e = 0I then (e)::(factors (n/e) e) else (factors n (e+1I))
    
let isprime n =
    let max = bigint(System.Math.Ceiling(System.Math.Sqrt(float(n))))
    if( (seq { 2I .. max } |> Seq.filter ( fun e -> n % e = 0I) |> Seq.length) > 0 ) then false else true;;

let rec listtoint (n:list<int>) acc =
    match n with
    |h::t -> listtoint t (acc*10I + bigint(h))
    |[] -> acc

let numdigit (n:bigint) =
    n.ToString().ToCharArray() |> Seq.fold ( fun acc e -> acc + 1) 0

let SumDigits(x:bigint) =            
    x.ToString().ToCharArray() |> Seq.fold ( fun acc e -> acc + System.Int32.Parse(e.ToString())) 0

let reverseandadd (n:bigint) = 
    n + (n.ToString().ToCharArray() |> Seq.toList |> List.rev |> List.fold ( fun acc e -> acc*10I + bigint.Parse(e.ToString())) 0I)

let listRotate l = 
    match l with
    |h::t -> List.append t [h]
    |[] -> []

let rec listRotations list times =
    if times = 0 then [list] else List.append [list] (listRotations (listRotate list) (times - 1))

let convtobin n =
    let rec inner n acc cont =
        match n with
        | 0 -> [0]
        | 1 -> [1]
        | n -> List.append (inner (n/2) acc (fun () -> cont())) [n % 2] 
    inner n [] (fun () -> [n % 2])

let squaredigitsandsum n =
    n.ToString() |> Seq.map (fun e -> Int32.Parse(e.ToString())) |> Seq.map (fun e -> e*e) |> Seq.sum

//Coprime generator for some number theory problems 71 onwards, need to reimplement this using continuations
let rec gencoprime m n max =
    if m > max then [] else
        List.append (List.append ([(m,n)]) (gencoprime (2*m - n) m max)) (List.append (gencoprime (2*m + n) m max) (gencoprime (m + 2*n) n max))

let primefactors n =
    [2I .. n/2I] |> List.filter (fun e -> isprime e) |> List.filter (fun e -> n % e = 0I)

let eulertotient n =
    match (isprime n) with
    | true -> n - 1I
    | false -> (primefactors n) |> List.fold (fun acc e -> acc*(e - 1I)/e) n
    