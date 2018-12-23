(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day23

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) |> List.ofArray
let rec repeat item = seq{ yield item; yield! repeat item }
let NL = System.Environment.NewLine
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = 
    BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

let parseLine  = 
    // pos=<24252823,1784286,56916822>, r=67997514
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        (grpi 1, grpi 2, grpi 3), grpi 4

let distance (x,y,z) (x',y',z') = abs(x - x') + abs(y - y') + abs(z - z')
   
let Part1 (input : string) =  // "result1" (*
    let bots = input |> toLines |> List.map parseLine
    let strongest = bots |> List.sortBy (fun bot -> snd bot) |> List.last
    let centre, radius = strongest
    let inRange = bots |> List.filter (fun b -> 
        let coord = fst b
        let dist = distance centre coord 
        dist <= radius)
    inRange |> List.length

//*)

    

(* ================ Part B ================ *)

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let Part2 result1 (input : string) = // "result2" (*
    let bots = input |> toLines |> List.map parseLine
    let corners =
        bots |> List.collect (fun ((x,y,z), rad) ->
            [ (x+rad,y,z); (x,y+rad,z); (x,y,z+rad);
                (x-rad,y,z); (x,y-rad,z); (x,y,z-rad)] )
    let bestCorners =
        corners 
        |> List.map (fun loc ->
            (loc, rangeCount bots loc))
        |> List.groupBy snd
        |> List.maxBy fst
        |> snd
    bestCorners 
        |> List.map (fun (loc, count)  -> ((loc, count), distance loc (0,0,0)))
    

//*)