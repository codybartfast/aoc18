(* a.cbf.pub/tx/8kM1fY7tJClZhTUfWEuLzypu8pcpqPLI_rUhrAgMO9M/data.html *)

module Day09

module Ring =

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
        if ring.Next = ring then failwith "Last pair standing"
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

open System.Text.RegularExpressions
open Ring

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

(* ================ Part A ================ *) 

let parseLine  = rxMatch "(\d+)\D+(\d+)" >> fun mtch ->
    let grpi idx = groupValue mtch idx |> int64
    grpi 1, grpi 2

let takeTurn marble scores turn player =
    if turn % 23L <> 0L then 
        (insert turn (marble.Next)), scores
    else
        let toRemove = back 7 marble
        let currentScore =
            match Map.tryFind player scores with
            | Some score -> score
            | None -> 0L
        let newScore = currentScore + turn + toRemove.Item
        (remove toRemove), (scores.Add (player, newScore))

let playGame playerCount topMarble =
    let rec playGame' marble scores turn  =  
        if turn > topMarble then scores
        else 
            let (marble', scores') = 
                takeTurn marble scores turn (turn % playerCount)
            playGame' marble' scores' (turn + 1L)
    playGame' (singleton 0L) Map.empty 1L
    |> Map.toSeq |> (Seq.maxBy snd) |> snd
    
let Part1 (input : string) =
    let (players, topMarble) = input |> parseLine
    playGame players topMarble


(* ================ Part B ================ *)

let Part2 result1 (input : string) = 
    let (players, topMarble) = input |> parseLine
    playGame players (topMarble * 100L)
