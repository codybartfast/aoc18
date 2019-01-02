(* a.cbf.pub/tx/0fuB_1QZn4ua1tGLf2dSoCwQVuusOAih8QiolDd9894/data.html *)

module Day25

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

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

let Part1 (input : string) =
    let coords = 
        input |> toLines |> List.map parseLine
    let constellations = 
        (Set.empty, coords)
        ||> List.fold (addCoord)
    constellations.Count

(* ================ Part B ================ *)

let Part2 result1 (input : string) = 
    "Happy New Year!"
