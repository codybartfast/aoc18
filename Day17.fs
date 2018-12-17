(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day17

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

type Sand = Dry | Wet | Soaked
type Ground = Sand of Sand | Clay
type Map = Ground[,]

let parseLine  = 
    //x=488, y=711..717
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

let exploreAccross map loc =  // increment left, right first?
    //if get map loc <> Some (Sand Dry) then failwith "oops" else
    let rec explore loc getNext =
        match get map loc with
        | None -> None
        | Some Clay -> Some (loc, Clay)
        | Some (Sand Dry) | Some (Sand Wet)->
            wet map loc
            match get map (below loc) with
            | None | Some (Sand Wet) -> None
            | Some Clay | Some (Sand Soaked) -> explore (getNext loc) getNext
            | Some (Sand Dry) -> Some ((below loc), Sand Dry)
        | Some ground -> None
    let left = explore loc left
    let right = explore loc right
    left, right        

let rec pour (map:Map) loc =
    let ground = get map loc
    if ground = Some (Sand Dry) then wet map loc
    let next = below loc
    match get map next with 
    | None -> None
    | Some ground -> 
        match ground with
        | Clay -> spread map loc
        | Sand Dry -> pour map next
        | Sand Wet -> spread map loc
        | Sand Soaked ->  spread map loc
and spread (map:Map) loc =
    let left, right = exploreAccross map loc
    match left, right with
    | Some (leftLoc, Sand Dry), None -> pour map leftLoc
    | None, Some (rightLoc, Sand Dry) -> pour map rightLoc
    | None, _ | _, None -> None
    | Some (leftLoc, Clay), Some (rightLoc, Clay) ->
            soak map leftLoc rightLoc
            spread map (above loc)
    | Some (leftLoc, Sand Dry), Some (_, Clay) -> pour map leftLoc
    | Some (_, Clay), Some (rightLoc, Sand Dry) -> pour map rightLoc
    | Some (leftLoc, Sand Dry), Some (rightLoc, Sand Dry) ->
            pour map rightLoc |> ignore
            pour map leftLoc

let measure (map:Map) =
    seq{for y in seq{map.GetLowerBound(1)..map.GetUpperBound(1)} do
            for x in seq{map.GetLowerBound(0)..map.GetUpperBound(0)} do
                yield (x,y)}
    |> Seq.map (fun (x,y) -> map.[x,y])
    |> Seq.sumBy (function  Sand Soaked -> 1 | _ -> 0)
            

let Part1 (input : string) =  // "result1" (*
    let map = 
        input |> toLines |>  List.map parseLine |> buildMap
    let startLoc = (500, map.GetLowerBound(1))
    pour map startLoc
    display map
    measure map



//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)