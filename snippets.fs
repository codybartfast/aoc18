module Snippets

let rec pairCombos = function
    // aoc18:02
    | [] | [_] -> []
    | head::tail -> 
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let rec permutations list =
    // aoc15:13
    let rec insertAlong i list = 
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)

module Ring =
    // aoc18:09
    type Ring<'a> = 
        {   Item : 'a
            mutable Prev : Ring<'a>
            mutable Next : Ring<'a> }

    let singleton item = 
        let rec s = {Item = item; Prev = s; Next = s }
        s
    
    let insert item ring = 
        let pair =  {Item = item; Prev = ring; Next = ring.Next }
        ring.Next.Prev <- pair
        ring.Next <- pair
        pair

    let remove ring =
        if ring.Next = ring then failwith "Oops! Last item standing"
        else
            let item = ring.Item
            ring.Prev.Next <- ring.Next
            ring.Next.Prev <- ring.Prev
            ring.Next

    let rec forward n ring = 
        match n with
        | 0 -> ring
        | n -> forward (n-1) ring.Next
        
    let rec back n ring = 
        match n with
        | 0 -> ring
        | n -> back (n-1) ring.Prev  
