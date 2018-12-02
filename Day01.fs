(* a.cbf.pub/tx/QSiqF_i8Ktfc-GLM13goqNWm3ZPfTGz2hn5wPoUW5TY/data.html *)

module Day01

open System

let lines (text:string) = 
    text.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofSeq 
let rec repeat items = seq{ yield! items; yield! repeat items }

(* ================ Part A ================ *) 

let Part1 input =
    input |> lines
    |> Seq.map int
    |> Seq.sum

(* ================ Part B ================ *)

let Part2 result1 input = 
    let changes = 
        input |> lines
        |> Seq.map int
        |> repeat
    
    ((0, Set.empty, false), changes)
    ||> Seq.scan (fun (frequency, knownFreqs, _isDuplicate) change ->
        let newFreq = frequency + change
        let isDup = knownFreqs.Contains newFreq
        (newFreq, knownFreqs.Add newFreq, isDup))
    |> Seq.find (fun (_, _, isDup) -> isDup)
