(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day09

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open Day08

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let rec repeat item = seq{ yield item; yield! repeat item }
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

type Game = { Marbles : int[] ; Length : int ; Current : int }

let parseLine  = rxMatch "(\d+)\D+(\d+)" >> fun mtch ->
    let grp idx = groupValue mtch idx
    let grpi = grp >> int
    grpi 1, grpi 2

type Marble = 
    {   Value : int
        mutable Previous : Marble
        mutable Next : Marble}

let rec zeroMarble = {Value = 0; Previous = zeroMarble; Next = zeroMarble }

let rec clockwise n (marble:Marble) = 
    match n with
    | 0 -> marble
    | n -> clockwise (n-1) marble.Next

let rec counterClockwise n (marble:Marble) = 
    match n with
    | 0 -> marble
    | n -> counterClockwise (n-1) marble.Previous    

let insert marble value =
    let newMarble = {Value = value; Previous = marble; Next = marble.Next}
    marble.Next.Previous <- newMarble 
    marble.Next <- newMarble 
    newMarble

let remove marble =
    marble.Next.Previous <- marble.Previous
    marble.Previous.Next <- marble.Next
    marble.Next

let makeMove marble scores turn player =
    if turn % 23 <> 0 then 
        (insert (clockwise 1 marble) turn), scores
    else
        let toRemove = counterClockwise 7 marble
        let removedValue = toRemove.Value
        let currentScore =
            match Map.tryFind player scores with
            | Some score -> score
            | None -> 0
        (remove toRemove)
            , scores.Add (player, currentScore + turn + removedValue)

let playGame playerCount topMarble =
    let rec play marble scores turn  =  
        if turn > topMarble then scores
        else 
            let (marble', scores') = 
                makeMove marble scores turn (turn % playerCount)
            play marble' scores' (turn+1)
    play zeroMarble Map.empty 1

    
let Part1 (input : string) =  //  "result1" (*
    let (players, topMarble) = input |>  parseLine
    //let (players, topMarble) = 9, 25
    let scores = playGame players topMarble
    scores
    |> Map.toSeq
    |> Seq.maxBy snd
    |> snd


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =   "result2" (*
    input |> toLines |> List.map parseLine




//*)