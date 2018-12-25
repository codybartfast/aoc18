(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day23

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Linq.Expressions
open System.Xml

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
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
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

type ABC = ABC of (int*int*int)

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let getRangeCorners ((x,y,z),r)  = 
    [ (x+r,y,z); (x,y+r,z); (x,y,z+r);
        (x-r,y,z); (x,y-r,z); (x,y,z-r)]    

let getNearest (cntr, _) corners =
    corners 
    |> Seq.map (fun corn -> corn, distance cntr corn)
    |> Seq.minBy snd

let getFurthest (cntr, _) corners =
    corners 
    |> Seq.map (fun corn -> corn, distance cntr corn)
    |> Seq.maxBy snd

let overlapDim (x1, y1) (x2, y2) (xn, yn) (xm, ym) =
    let dist (a,b) (c,d) = abs(a-c) + abs(b-d)
    let rad = (dist (x1,y1) (xm,ym)) +  (dist (x2,y2) (xn,yn))
    let cntrDist = dist (x1,y1) (x2,y2)
    let overlap = rad - cntrDist
    let half = overlap / 2
    let rmn = overlap % 2
    let xDiff = (x2 - x1)
    let yDiff = (y2 - y1)
    let xCloser = abs xDiff < abs yDiff
    let (xDelta, yDelta) =
        if xCloser
            then half, half+rmn
            else half+rmn, half
    let xSign = if xDiff = 0 then 1 else (xDiff/abs xDiff)
    let ySign = if yDiff = 0 then 1 else (yDiff/abs yDiff)

    let shift = if xCloser then abs xDiff else abs yDiff

    let sx, sy = 
        if xCloser 
                 // smaller    // bigger
            then (xn - (shift * xSign)), (yn + (shift * ySign))
            else (xn + (shift * xSign)), (yn - (shift * ySign))
                 // bigger     // smaller
    let one = (xn+(xDelta*xSign), yn+(yDelta*ySign))
    let two =
        if xCloser 
            then (sx-(xDelta*xSign), sy+(yDelta*ySign))
            else (sx+(xDelta*xSign), sy-(yDelta*ySign))
    [one; two]

let overlapCorners bot1 bot2 nearest myNearest =
    let (cntr1,r1), (cntr2,r2) = bot1, bot2
    let (x1,y1,z1) = cntr1
    let (x2,y2,z2) = cntr2
    let xn, yn, zn = nearest
    let xm, ym, zm = myNearest
    let distance = distance cntr1 cntr2
    let overlap = (r1 + r2) - distance
    let XYs = overlapDim (x1, y1) (x2, y2) (xn, yn) (xm, ym)
    let YZs = overlapDim (y1, z1) (y2, z2) (yn, zn) (ym, zm)
    let XZs = overlapDim (x1, z1) (x2, z2) (xn, zn) (xm, zm)
    let xyCorners = XYs |> List.map (fun (x,y) -> (x,y,zn))
    let yzCorners = YZs |> List.map (fun (y,z) -> (xn,y,z))
    let xzCorners = XZs |> List.map (fun (x,z) -> (x,yn,z))
    List.collect id [
        xyCorners; 
        yzCorners; 
        //xzCorners
        ] 


let consider bot1 bot2 =
    let nowt = []
    let _, rad = bot1
    if bot1 = bot2  then nowt else
    let corners2 = getRangeCorners bot2
    let (bot2Nearest, nDist) = getNearest bot1 corners2
    if nDist > rad then nowt else
    let (_, fDist) = getFurthest bot1 corners2
    if fDist <= rad then nowt else
    corners2

let pointsOfInterest allBots bot = seq{
    let corners = getRangeCorners bot
    yield! corners
    yield!
        allBots 
        |> Seq.collect (consider bot)}

let allPoi allBots =
    allBots
    |> Seq.collect (pointsOfInterest allBots)

let mutable count = 0
let test (cntr1, r1) (cntr2, r2) loc =
    count <- (print (count + 1))
    let actual = (r1 + r2) - ((distance cntr1 loc) + (distance cntr2 loc))
    if actual < 0 || actual > 1 then failwith "oops"

let Part2 result1 (input : string) = // "result2" 
    let bots = input |> toLines |> List.map parseLine
    //allPoi bots |> Seq.length
    let bot1 = bots.Item 0
    let bot2 = bots.Item 5
    
    //let bot1 = ((0,0,0), 10)
    //let bot2 = ((5,11,0), 10)
    //let bot1 = ((0,0,0), 10)
    //let bot2 = ((15,-2,0), 10)
    let bot1 = ((0,0,0), 10)
    let bot2 = ((15,-2, 1), 10)

    let (nearest, _) = getNearest bot1 (getRangeCorners bot2)
    let (myNearest, _) = getNearest bot2 (getRangeCorners bot1)
    let ocs = overlapCorners bot1 bot2 (print nearest) myNearest
    printfn "---"
    ocs |> List.iter(fun oc ->  test bot1 bot2 (print oc))
   

    


//