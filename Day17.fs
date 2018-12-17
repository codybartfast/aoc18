(* a.cbf.pub/tx/NLf5y4Wj_rG3tNmzlsmXhN9l2OOKaEKj90koSOVTgrg/data.html *)

module Day17

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

(* ================ Part A ================ *) 

type Sand = Dry | Wet | Soaked
type Ground = Sand of Sand | Clay
type Map = Ground[,]

let parseLine  = 
    rxMatch "(.)=(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grp 1, grpi 2, grpi 3, grpi 4

let buildMap veinInput =
    let veinRanges =
        veinInput
        |> List.map (fun (axis, value, rangeStart, rangeEnd) ->
            let range1 = (value, value)
            let range2 = (rangeStart, rangeEnd)
            match axis with
            |"x" -> range1, range2
            |"y" -> range2, range1
            | _ -> failwith "oops")
    let sampleX = veinRanges.Head  |> fst |> fst
    let sampleY = veinRanges.Head  |> snd |> fst
    let (minX, maxX, minY, maxY) =
        ((sampleX, sampleX, sampleY, sampleY), veinRanges)
        ||> List.fold (fun (minX, maxX, minY, maxY) (xRange, yRange) ->
            (min minX (fst xRange)), (max maxX (snd xRange)),
                (min minY (fst yRange)), (max maxY (snd yRange)))    
    let map = 
        Array2D.createBased 
            (minX-1) minY (3 + maxX - minX) (1 + maxY - minY) (Sand Dry)
    veinRanges
    |> Seq.iter (fun ((x, X), (y, Y)) ->
        seq{for x in seq{x..X} do for y in seq{y..Y} do yield (x,y)}
        |> Seq.iter (fun (x,y) -> map.[x,y] <- Clay))
    map

let display (map:Map) =
    let symbol = function   
        | Clay -> '#'
        | Sand Dry -> '.'
        | Sand Wet -> '|'
        | Sand Soaked -> '~'
    let diplayLine line = seq{    
            yield! seq{ 
                for x in seq{map.GetLowerBound(0)..map.GetUpperBound(0)}
                    do yield symbol map.[x,line]}
            yield '\n'}
    let symbols = 
        seq{map.GetLowerBound(1)..map.GetUpperBound(1)}
        |> Seq.collect diplayLine
    printfn "%s" (toString symbols)
    map

let above (x,y) = (x, y-1)
let left (x,y) = (x-1, y)
let right (x,y) = (x+1, y)
let below (x,y) = (x, y+1)

let get (map:Map) (x,y) = 
    if x >= map.GetLowerBound(0) 
        && x <= map.GetUpperBound(0)
        && y >= map.GetLowerBound(1)
        && y <= map.GetUpperBound(1)
    then Some map.[x,y] else None

let wet (map:Map) (x,y) = map.[x,y] <- Sand Wet

let soak (map:Map) (left, y) (right, y') =
    if y <> y' then failwith "oops!" else
    seq{left+1..right-1}
    |> Seq.iter (fun x -> map.[x,y] <- Sand Soaked)

let exploreAccross map loc = 
    let rec explore loc getNext =
        match get map loc with
        | Some Clay -> Some (loc, Clay)
        | Some (Sand Dry) | Some (Sand Wet)->
            wet map loc
            match get map (below loc) with
            | Some (Sand Wet) -> None
            | Some (Sand Dry) -> Some ((below loc), Sand Dry)
            | Some Clay | Some (Sand Soaked) -> 
                explore (getNext loc) getNext
        | Some ground -> None
    let left = explore loc left
    let right = explore loc right
    left, right        

let rec pour (map:Map) loc =
    let ground = get map loc
    if ground = Some (Sand Dry) then wet map loc
    let next = below loc
    match get map next with 
    | Some ground -> 
        match ground with
        | Sand Dry -> pour map next
        | _ ->  spread map loc
    | _ -> ()

and spread (map:Map) loc =
    let left, right = exploreAccross map loc

    match left with 
    | Some (leftLoc, Sand Dry) -> pour map leftLoc 
    | _ -> ()

    match right with
    | Some (rightLoc, Sand Dry) -> pour map rightLoc 
    | _ -> ()

    match left, right with
    | Some (leftLoc, Clay), Some (rightLoc, Clay) ->
            soak map leftLoc rightLoc
            spread map (above loc)
    | _ -> ()

let measure (map:Map) includeWet =
    seq{for y in seq{map.GetLowerBound(1)..map.GetUpperBound(1)} do
            for x in seq{map.GetLowerBound(0)..map.GetUpperBound(0)} do
                yield (x,y)}
    |> Seq.map (fun (x,y) -> map.[x,y])
    |> Seq.sumBy (fun ground ->
        match ground, includeWet with
        | Sand Soaked, _ -> 1 
        | Sand Wet, true -> 1
        | _ -> 0)

let Part1 (input : string) =
    let map = input |> toLines |>  List.map parseLine |> buildMap
    let startLoc = (500, map.GetLowerBound(1))
    pour map startLoc
    //display map
    measure map true
   
(* ================ Part B ================ *)

let Part2 result1 (input : string) = 
    let map =input |> toLines |>  List.map parseLine |> buildMap
    let startLoc = (500, map.GetLowerBound(1))
    pour map startLoc
    //display map
    measure map false
