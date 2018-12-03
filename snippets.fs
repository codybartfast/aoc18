module Snippets

let rec pairCombos = function
    | [] | [_] -> []
    | head::tail -> 
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let rec permutations list =
    let rec insertAlong i list = 
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)