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

let getEdges bots =
    let extremes nums =
        Seq.min nums, Seq.max nums
    let X = 
        bots
        |> Seq.map (fun ((x,_,_),_) -> x) |> extremes
    let Y = 
        bots
        |> Seq.map (fun ((_,y,_),_) -> y) |> extremes
    let Z = 
        bots
        |> Seq.map (fun ((_,_,z),_) -> z) |> extremes
    (X, Y, Z)

let rangeCount bots sample =
    let inRange = 
        bots 
        |> Seq.filter (fun (coord, radius) -> 
            let dist = distance sample coord 
            dist <= radius)
        |> Seq.length
    inRange

let checkAll low high bots =
    let (x,y,z), (x',y',z') = low, high
    let bests = 
        [for z in [z..z'] do for y in [y..y'] do for x in [x..x'] do yield (x,y,z) ]
        |> List.map (fun coord -> (coord, rangeCount bots coord))
        |> List.groupBy (fun (coord, count) -> count)
        |> Seq.maxBy fst
        |> snd
     
    bests
    |> List.map (fun (coord, _) -> (coord, distance coord (0,0,0)))
    |> List.minBy (fun (coord, dist) -> dist)
    //|> fst
    //|> (rangeCount bots)

let rec split low high bots =
    if distance low high  < 100 then checkAll low high bots else
    let (x,y,z), (x',y',z') = low, high
    let xInt = (x'-x)/10
    let yInt = (y'-y)/10
    let zInt = (z'-z)/10
    let xs = [x..xInt..(x'+xInt)]
    let ys = [y..xInt..(y'+yInt)]
    let zs = [z..xInt..(z'+zInt)]
    let pick =
        [for z in zs do for y in ys do for x in xs do yield (x,y,z)]
        |> Seq.map (fun sample -> (sample, rangeCount bots sample))
        |> Seq.maxBy(fun (_, count ) -> count)
    let (px, py, pz) = fst (print pick)
    let low = (px-(xInt*1), py-(yInt*1), pz-(zInt*1))
    let high = (px+(xInt*1), py+(yInt*1), pz+(zInt*1))
    split low high bots

let Part2 result1 (input : string) = // "result2" (*
    let bots = input |> toLines |> List.map parseLine
    let (xMin, xMax), (yMin, yMax), (zMin, zMax) = getEdges bots
    let lowCorn, highCorn = (xMin, yMin, zMin), (xMax, yMax, zMax)
    split lowCorn highCorn bots


//*)