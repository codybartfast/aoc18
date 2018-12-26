(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day23

// #nowarn "0025"

open System
open System.Text.RegularExpressions


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

type Bot = {
    Centre : int*int*int
    Radius : int
    Side1 : int
    Side2 : int
    BackSide1 : int
    BackSide2 : int }

let parseLine  = 
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int64
        (grpi 1, grpi 2, grpi 3), grpi 4

let distance (x,y,z) (x',y',z') = abs(x - x') + abs(y - y') + abs(z - z')
   
let Part1 (input : string) = 
    let bots = input |> toLines |> List.map parseLine
    let strongest = bots |> List.sortBy (fun bot -> snd bot) |> List.last
    let centre, radius = strongest
    let inRange = bots |> List.filter (fun b -> 
        let coord = fst b
        let dist = distance centre coord 
        dist <= radius)
    inRange |> List.length

(* ================ Part B ================ *)

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let leftSlope ((x,y),r) = (x-y)-r
let rightSlope ((x,y), r) = (x-y)+r
let leftBackslope ((x,y), r) = (x+y)-r
let rightBackslope ((x,y), r) = (x+y)+r

//let slopeIntersects dmnd1 dmnd2 =
//    let left1 = leftSlope dmnd1
//    let right1 = rightSlope dmnd1
//    let left2 = leftSlope dmnd2
//    let right2 = rightSlope dmnd2
//    let xxx = (left2 <= left1 && left1 <= right2)
//                || (left1 <= left2 && left2 <= right1)
//    xxx
    
//let backslopeIntersects dmnd1 dmnd2 =
//    let left1 = leftBackslope dmnd1
//    let right1 = rightBackslope dmnd1
//    let left2 = leftBackslope dmnd2
//    let right2 = rightBackslope dmnd2
//    let xxx = (left2 <= left1 && left1 <= right2)
//                || (left1 <= left2 && left2 <= right1)
//    xxx

//let intersects dmnd1 dmnd2 = 
//    slopeIntersects dmnd1 dmnd2
//    && backslopeIntersects dmnd1 dmnd2

let doIntersect (cntr1,r1) (cntr2,r2) =
    let radtotal = r1 + r2
    let distance = distance cntr1 cntr2
    distance <= radtotal

let getCorners dmnd1 dmnd2 =
    let corners slope otherNear otherFar fct =
        let distN = otherNear - slope
        let countN = distN - (distN / 2L)
        let c1 = (slope + countN), (fct * countN)
        let distF = otherFar - slope     
        let countF = distF / 2L
        let c2 = (slope + countF), (fct * countF)
        [c1; c2]
    let leftSlope = leftSlope dmnd1
    let rightSlope = rightSlope dmnd1
    let leftBackslope = leftBackslope dmnd2
    let rightBackslope = rightBackslope dmnd2
    Seq.collect id [
        corners leftSlope leftBackslope rightBackslope 1L
        corners rightSlope leftBackslope rightBackslope 1L
        corners rightBackslope rightSlope leftSlope -1L
        corners leftBackslope rightSlope leftSlope -1L]


let Part2 result1 (input : string) = // "result2" 
    let bots = input |> toLines |> List.map parseLine
    seq{ for bot1 in bots do for bot2 in bots do yield (bot1, bot2)}
    |> Seq.collect(fun (bot1,bot2) -> 
        let intersect = doIntersect bot1 bot2
        if (not intersect) then Seq.empty else
        let (x1, y1, z1), r1 = bot1
        let (x2, y2, z2), r2 = bot2
        Seq.collect id [
            getCorners ((x1, y1), r1) ((x2, y2), r2) 
                |> Seq.collect (fun (x,y) -> [(x,y,z1); (x,y,z2)])
            getCorners ((y1, z1), r1) ((y2, z2), r2)
                |> Seq.collect (fun (y,z) -> [(x1,y,z); (x2,y,z)])
            getCorners ((x1, z1), r1) ((x2, z2), r2)
                    |> Seq.collect (fun (x,z) -> [(x,y1,z); (x,y2,z)])])
        //|> Seq.map (rangeCount bots)
        //|> Seq.max

        |> Seq.map (fun loc -> loc, rangeCount bots loc)
        |> Seq.filter (fun (loc, count) -> count = 898)
        |> Seq.map (fun (loc, count) -> loc, distance loc (0L,0L,0L))
        |> Seq.minBy snd

