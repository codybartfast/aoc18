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
type Loc = int*int*int
type Vct = int*int*int
type NeighbourInfo = Point*Vct*Vct

let add (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
let sub (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)
let mul (x1,y1,z1) (x2,y2,z2) = (x1*x2, y1*y2, z1*z2)

let scale m (x,y,z) = (m*x, m*y, m*z)
let sum (x,y,z) = x+y+z

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
    
let getNeighbours (_, point) : NeighbourInfo list= 
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

let getPoi (cntr1:Loc) (slack:int) (corner:Loc) ((_,vct1, vct2):NeighbourInfo) =
    let v2Count = slack / 2
    let v1Count = slack - v2Count
    let crnrVect = sub corner cntr1
    let crnrVectByVct2 = (mul crnrVect vct2)
    let poi =
        if sum crnrVectByVct2 > 1 then
            (add
                (add corner (scale v1Count vct1))
                (scale v2Count vct2))
        else
            let distAxis = -1 * (sum crnrVectByVct2)
            (add
                (add corner (scale (distAxis + v1Count) vct1))
                (scale (distAxis + v2Count) vct2))
    poi
    
let getPoiFromCorner cntr1 slack ((corner,_), neighbourInfos) =
    neighbourInfos
    |> List.map (fun neighbourInfo -> getPoi cntr1 slack corner neighbourInfo)

let getPoiForBots (bot1, bot2) =
    //if bot1 = bot2 then [] else
    let (c1, r1), (c2, r2) = bot1, bot2
    //if r2 > r1 then [] else
    let slack = -1 + r1 + r2 - distance c1 c2
    if slack <= 0 then Seq.empty else

    let insideCorners = getInsideCornders bot1 bot2
    let crnNeighbourInfos =
        insideCorners
        |> List.map (fun cnr -> cnr, chooseNeighbours cnr insideCorners)
    let xxx= 
        crnNeighbourInfos
        |> Seq.collect (getPoiFromCorner c1 slack)
    xxx

let Part2 result1 (input : string) = // "result2" 
    // not looking at corners!
    let bots = input |> toLines |> List.map parseLine
    let max, locations =
        seq{ for bot1 in bots do for bot2 in bots do yield (bot1, bot2)}
        |> Seq.collect getPoiForBots
        |> Seq.map (fun loc -> loc, rangeCount bots loc)
        |> Seq.groupBy snd
        |> Seq.maxBy fst
    let locations = List.ofSeq locations
    let closest =
        locations
        |> List.map (fst>>(distance (0,0,0)))
        |> List.min
    max, List.length locations, closest
    