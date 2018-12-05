(* a.cbf.pub/tx/UALiYY7YM8d-TnYKH9gJoj3pfhUo8OdwhiUvG9G6WME/data.html *)

module Day05

(* ================ Part A ================ *) 

let toPolymer (input : string) = 
    input
    |> Seq.map int 
    |> (List.ofSeq)

let collapsedLength polymer =
    let rec selectiveCopy source target = 
        match source, target with
            | [], target -> target
            | sHead::sTail, [] -> selectiveCopy sTail [sHead]
            | sHead::sTail, tHead::tTail ->  
                if sHead - tHead |> abs  = 32 
                    then selectiveCopy sTail tTail
                    else selectiveCopy sTail (sHead::target)
    selectiveCopy polymer []
    |> List.length   

let Part1 = toPolymer >> collapsedLength 

(* ================ Part B ================ *)

let removeType polymer unitType =
    polymer
    |> List.filter (fun i -> i <> unitType && i <> (unitType + 32))

let Part2 result1 input = 
    [(int 'A')..(int 'Z')]
    |> List.map ((removeType (toPolymer input)) >> collapsedLength)
    |> List.min
