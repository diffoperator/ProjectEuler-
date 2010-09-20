module Utils

let sum list =
   let rec loop list acc =
       match list with
       | head :: tail -> loop tail (acc + head)
       | [] -> acc
   loop list 0

let reduceAddNumList acc i = acc + i

let primefactors n = 
    let max = int( System.Math.Ceiling ( System.Math.Sqrt (n)))
    let rec factorlist n max =
        if n = 1.0 then [] else
            let a = [2 ..max] |> List.find (fun x -> n % float(x) = 0.0)
            a :: factorlist (n / float(a)) max
    factorlist n max

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