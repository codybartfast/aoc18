(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day25

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

type Coord = int * int * int * int

let parseLine  = 
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2, grpi 3, grpi 4

let distance (x,y,z,t) (x',y',z',t') =
    abs (x - x') + abs (y - y') + abs (z - z') + abs (t - t')

let inRangeStar coord star = distance star coord <= 3
    
let inRangeConst coord (constellation : Set<Coord>)   =
    let closeStar =
        constellation
        |> Seq.tryFind (inRangeStar coord)
    match closeStar with
    | Some _star -> Some constellation
    | None -> None

let addCoord (constellations : Set<Set<Coord>>) coord : Set<Set<Coord>>=
    let inRangeConsts = 
        constellations
        |> Seq.choose (inRangeConst coord)
        |> List.ofSeq
    if inRangeConsts.IsEmpty then
        let newConst = Set.singleton coord
        constellations.Add newConst 
    else
        let constellations = 
            (constellations, inRangeConsts)
            ||> List.fold (fun consts inRange -> Set.remove inRange consts)
        let newConst = 
            (Set.unionMany inRangeConsts).Add coord
        constellations.Add newConst

let Part1 (input : string) =  // "result1" (*
    let coords = 
        input |> toLines |> List.map parseLine
    let constellations = 
        (Set.empty, coords)
        ||> List.fold (addCoord)
    constellations.Count

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)