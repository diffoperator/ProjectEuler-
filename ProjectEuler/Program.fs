// Learn more about F# at http://fsharp.net

module EntryPoint

open System

[<EntryPoint>]
let main (args : string[]) =
    Seq.iter (fun e -> Console.WriteLine(e.ToString())) Utils.trianglenumbergen
    let input = Console.ReadLine()
    0
