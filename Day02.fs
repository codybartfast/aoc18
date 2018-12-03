(* a.cbf.pub/tx/VnSPvOk5uwNoMQFXsdl2Apm60iyqDh-2PVGk39IVcNA/data.html *)

module Day02

open System

let toLines (text:string) = 
    text.Split('\n', StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofSeq 
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let filterCount predicate = Seq.filter predicate >> Seq.length

(* ================ Part A ================ *) 

let hasRepeatCount count = 
    Seq.countBy id
    >> Seq.map snd
    >> Seq.contains count

let Part1 input = 
    let ids = input |> toLines
    (*) (ids |> filterCount (hasRepeatCount 2)) 
        (ids |> filterCount (hasRepeatCount 3))

(* ================ Part B ================ *)

let hasOneDiff (id1, id2) = 
    (id1, id2)
    ||> Seq.map2 (fun c1 c2 -> c1 <> c2)
    |> filterCount id
    |> (function 1 -> Some (id1, id2) | _ -> None)

let rec pairCombos = function
    | [] | [_] -> []
    | head::tail -> 
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let getMatchingChars (str1, str2) =
    (str1, str2)
    ||> Seq.map2 (fun c1 c2 -> 
        if c1 = c2 then Some c1 else None)
    |> Seq.choose id
    |> toString

let Part2 result1 = 
    toLines
    >> pairCombos
    >> List.choose hasOneDiff
    >> List.exactlyOne
    >> getMatchingChars
