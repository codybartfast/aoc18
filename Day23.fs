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

type Point = A|N|S|W|E|B

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let doIntersect (cntr1,r1) (cntr2,r2) =
    let radtotal = r1 + r2
    let distance = distance cntr1 cntr2
    distance <= radtotal

let getCorners ((x,y,z), r) =
    [(  (x,y,z+r), A);
            ((x,y+r,z), N);
        ((x-r,y,z), W); ((x+r,y,z), E);
            ((x,y-r,z), S);
        ((x,y,z-r), B);]

let getInsideCornders bot1 bot2 =
    let (c1, r1) = bot1
    getCorners bot2
    |> List.filter(fun (loc,crn) -> distance c1 loc <= r1)
    
let getNeighbours (_, point) = 
    match point with
    | A -> [    (N, (0,0,1), (0,-1,0));
                (W, (0,0,1), (-1,0,0));
                (E, (0,0,1), (1,0,0));
                (S, (0,0,1), (0,1,0));]
    | N -> [    (A, (0,1,0), (0,0,-1)); 
                (W, (0,1,0), (-1,0,0));
                (E, (0,1,0), (1,0,0));
                (B, (0,1,0), (0,0,1)); ]
    | W -> [    (A, (1,0,0), (0,0,-1)); 
                (N, (1,0,0), (0,1,0));
                (S, (1,0,0), (0,1,0));
                (B, (1,0,0), (0,0,1)); ]
    | E -> [    (A, (-1,0,0), (0,0,-1)); 
                (N, (-1,0,0), (0,-1,0));
                (S, (-1,0,0), (0,1,0));
                (B, (-1,0,0), (0,0,1)); ]
    | S -> [    (A, (0,-1,0), (0,0,-1)); 
                (W, (0,-1,0), (-1,0,0));
                (E, (0,-1,0), (1,0,0));
                (B, (0,-1,0), (0,0,1)); ]
    | B -> [    (N, (0,0,-1), (0,-1,0));
                (W, (0,0,-1), (-1,0,0));
                (E, (0,0,-1), (1,0,0));
                (S, (0,0,-1), (0,1,0)); ]
    
let chooseNeighbours crn ignoreCrns =
    let ignorePts = 
        ignoreCrns
        |> List.map (fun (_,pt) -> pt)
    getNeighbours crn
    |> List.filter (fun (point, _,_) -> 
        not (List.contains point ignorePts))

let getPoiFromCorner cntr1 (corner, neighbours) =
    

let getPoi (bot1, bot2) =
    //if bot1 = bot2 then [] else
    let (c1, r1), (c2, r2) = bot1, bot2
    //if r2 > r1 then [] else
    let slack = -1 + r1 + r2 - distance c1 c2
    if slack <= 0 then [] else

    let insideCorners = getInsideCornders bot1 bot2
    let neighbourInfo =
        insideCorners
        |> List.map (fun cnr -> cnr, chooseNeighbours cnr insideCorners)
    [1]

let Part2 result1 (input : string) = // "result2" 
    let bots = input |> toLines |> List.map parseLine
    seq{ for bot1 in bots do for bot2 in bots do yield (bot1, bot2)}
    |> Seq.collect getPoi