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

let add (x,y,z) (x',y',z') = x+x',y+y',z+z'
let mul m (x,y,z) = (x*m, y*m, z*m)    
let div (x,y,z) d = (x/d, y/d, z/d)    

let A = (1,-1,1)
let B = (1,1,-1)
let C = (-1,1,1)

let AB, BC, CA = (add A B), (add B C), (add C A)

let toABC (x,y,z) =
    let p1 = (mul x (1, 1, 0))
    let p2 = (mul y (0, 1, 1))
    let p3 = (mul z (1, 0, 1))
    (add (add p1 p2) p3)

let fromABC (a,b,c) =
    let p1 = (mul a A)
    let p2 = (mul b B)
    let p3 = (mul c C)
    (div (add (add p1 p2) p3) 2)



let overlapCornersFromPoints bot1 bot2 near2 =
    let (cntr1, rad1) = bot1
    let (cntr2, rad2) = bot2
    let reqdSteps = rad1 + rad2 - (distance cntr1 cntr2)

    let (x1, y1, z1) = cntr1
    let (x2, y2, z2) = cntr2

    let stepX = if x2 >= x1 then 1 else -1
    let stepY = if y2 >= y1 then 1 else -1
    let stepZ = if z2 >= z1 then 1 else -1
    let fullSteps = reqdSteps / 3

    let nx, ny, nz = near2
    []

//let overlapCornersFromPoints myNearest theirNearest =
//    let mx,my,mz = myNearest   
//    let tx,ty,tz = theirNearest
//    let myABC = toABC myNearest
//    let theirABC = toABC theirNearest
//    let ma,mb,mc = myABC   
//    let ta,tb,tc = theirABC

//    //let deltaA, deltaB, deltaC = (ma-ta, mb-tb, mc-tc)
//    let deltaA, deltaB, deltaC = (mx-tx, my-ty, mz-tz)
//    let vectors = [
//        (deltaA, 0, 0); (0, deltaB, 0); (0, 0, deltaC)
//        (0, deltaB, deltaC); (deltaA, 0, deltaC); (deltaA, deltaB, 0);
//        //(deltaA, deltaB, deltaC)
//        ]
//    let abcCorners = 
//        vectors |> List.map (add theirABC)
//    abcCorners |> List.map fromABC
//    //let other = (add theirABC (deltaA, 0, deltaC))
//    //[myNearest; fromABC (other)]

let overlapCorners bot1 corners1 bot2 =
    let nowt = []
    let _, rad = bot1
    if bot1 = bot2  then nowt else
    let corners2 = getRangeCorners bot2
    let (bot2Nearest, nDist) = getNearest bot1 corners2
    if nDist > rad then nowt else
    let (_, fDist) = getFurthest bot1 corners2
    if fDist <= rad then nowt else
    let (bot1Nearest, _) = getNearest bot2 corners1
    overlapCornersFromPoints bot1 bot2 bot2Nearest 

let pointsOfInterest allBots bot = seq{
    let corners = getRangeCorners bot
    yield! corners
    yield!
        allBots 
        |> Seq.collect (overlapCorners bot corners)}

let allPoi allBots =
    allBots
    |> Seq.collect (pointsOfInterest allBots)

let Part2 result1 (input : string) = // "result2" (*
    let bots = input |> toLines |> List.map parseLine
    let allCorners = bots |> Seq.collect getRangeCorners
    let abcCorners = allCorners |> Seq.map toABC |> List.ofSeq
    let allA = 
        abcCorners
        |> List.map (fun (a,_,_) -> a)
        |> List.distinct
    let allB = 
        abcCorners
        |> List.map (fun (_,b,_) -> b)
        |> List.distinct
    let allC = 
        abcCorners
        |> List.map (fun (_,_,c) -> c)
        |> List.distinct
    
    seq{for a in allA do  for c in allC do yield (a,0,c)}
    |> Seq.map fromABC
    |> Seq.map (rangeCount bots)
    |> Seq.max

//*)