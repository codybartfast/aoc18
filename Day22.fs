(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day22

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    //|> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) 
    |> List.ofArray
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

type Location = int*int
type Type = Rocky | Narrow | Wet
type Region = {Location : Location; Type : Type; Erosion : int}
type Cave = Map<Location, Region>

let parse text  = 
    let lines = text |> toLines
    let depth = rxMatch "\d+" lines.[0] |> (fun m -> m.Value |> int)
    let (x,y) = rxMatch "(\d+),(\d+)" lines.[1] |> (fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2)
    (depth, (x, y))

let display (cave:Cave) tLoc =
    let symbol {Type=typ} = 
        match typ with
        | Rocky -> '.'
        | Wet -> '='
        | Narrow -> '|'
    let coords = cave |> Map.toList |> List.map fst
    let maxX = coords |> List.map fst |> List.max
    let maxY = coords |> List.map snd |> List.max
    let array = 
        Array.init (maxY+1)  (fun  y -> 
            Array.init (maxX+1) (fun x -> 
                cave.[(x,y)] |> function
                    | {Location = (0,0)} -> 'M'
                    | {Location = loc} when loc = tLoc-> 'T'
                    | r -> symbol r))
    array
    |> Array.iter (fun (line:char[]) -> printfn "%s" (toString line))
    cave

let getType erosion =
    match erosion % 3 with
    | 0 -> Rocky
    | 1 -> Wet
    | 2 -> Narrow

let newRegion depth tLoc (cave:Cave) loc =
    let erosion  =
        let (x,y) = loc
        let geoIndex =
            if loc = (0,0) || loc = tLoc then 0 else
            if y = 0 then x * 16807 else
            if x = 0 then y * 48271 else
            cave.[x-1,y].Erosion * cave.[x,y-1].Erosion
        (geoIndex + depth) % 20183
    {Location = loc; Type = (getType erosion); Erosion = erosion}

let buildCave depth dimensions tLoc  : Cave =
    let (X,Y) = dimensions
    ((Map.empty),
        [for y in 0..Y do for x in 0..X do yield (x,y)])
    ||> List.fold (fun cave loc ->
        cave
        |> Map.add loc (newRegion depth tLoc cave loc))

let assessRisk (cave:Cave) =
    cave
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sumBy (fun {Type=typ} -> 
        match typ with Rocky -> 0 | Wet -> 1 | Narrow -> 2)

let Part1 (input : string) =  // "result1" 
    //let depth, targetLoc = 510, (10,10)
    //let cave = buildCave depth targetLoc targetLoc
    let depth, targetLoc = parse input
    let cave = buildCave depth targetLoc targetLoc
    display cave targetLoc
    |> assessRisk



//

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)