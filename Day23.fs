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

type Bot = ((int*int*int)*int)

let parseLine  = 
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        (grpi 1, grpi 2, grpi 3), grpi 4

let distance (x,y,z) (x',y',z') = abs(x - x') + abs(y - y') + abs(z - z')
let distance2D (x,y) (x',y') = abs(x - x') + abs(y - y')
   
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
type Plane = XY|YZ|XZ
type Loc = int*int*int
type Vct = int*int
type ProjectInfo = Point*Plane*Vct*Vct

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let doIntersect (cntr1,r1) (cntr2,r2) =
    let radtotal = r1 + r2
    let distance = distance cntr1 cntr2
    distance <= radtotal

let getCorners ((x,y,z), r) =
    [(  (x,y,z-r), A);
            ((x,y-r,z), N);
        ((x-r,y,z), W); ((x+r,y,z), E);
            ((x,y+r,z), S);
        ((x,y,z+r), B);]

let getInsideCornders bot1 bot2 =
    let (c1, r1) = bot1
    getCorners bot2
    |> List.filter(fun (loc,crn) -> distance c1 loc <= r1)
    
let getProjectInfos (_, point) : ProjectInfo list= 
    match point with
    | A -> [    (N, YZ, (0,1), (-1,0));
                (W, XZ, (0,1), (-1,0));
                (E, XZ, (0,1), (1,0));
                (S, YZ, (0,1), (1,0));]
    | N -> [    (A, YZ, (1,0), (0,-1)); 
                (W, XY, (0,1), (-1,0));
                (E, XY, (0,1), (1,0));
                (B, YZ, (1,0), (0,1)); ]
    | W -> [    (A, XZ, (1,0), (0,-1)); 
                (N, XY, (1,0), (0,-1));
                (S, XY, (1,0), (0,1));
                (B, XZ, (1,0), (0,1)); ]
    | E -> [    (A, XZ, (-1,0), (0,-1)); 
                (N, XY, (-1,0), (0,-1));
                (S, XY, (-1,0), (0,1));
                (B, XZ, (-1,0), (0,1)); ]
    | S -> [    (A, YZ, (-1,0), (0,-1)); 
                (W, XY, (0,-1), (-1,0));
                (E, XY, (0,-1), (1,0));
                (B, YZ, (-1,0), (0,1)); ]
    | B -> [    (N, YZ, (0,-1), (-1,0));
                (W, XZ, (0,-1), (-1,0));
                (E, XZ, (0,-1), (1,0));
                (S, YZ, (0,-1), (1,0)); ]
    
let chooseNeighbours crn ignoreCrns =
    let ignorePts = 
        ignoreCrns
        |> List.map (fun (_,pt) -> pt)
    getProjectInfos crn
    |> List.filter (fun (point,_, _,_) -> 
        not (List.contains point ignorePts))

let getPoi2D (c1, r1) crnr vct1 vct2 =
    let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
    let sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
    let mul (x1,y1) (x2,y2) = (x1*x2, y1*y2)
    let scale m (x,y) = (m*x, m*y)
    let sum (x,y) = x+y

    let dist = distance2D c1 crnr
    let steps = r1 - dist
    let v2Count = steps / 2
    let v1Count = steps - v2Count
    let crnrVect = sub crnr c1
    let crnrVectByVct1 = (mul crnrVect vct1)
    let crnrVectByVct2 = (mul crnrVect vct2)
    let vct1MovesOut = sum crnrVectByVct1 > 1
    let vct2MovesOut = sum crnrVectByVct2 > 1
    let poi =
        let distAxis = 
            match vct1MovesOut, vct2MovesOut with
            | true, true -> 0
            | false, true -> abs (sum crnrVectByVct1)
            | true, false -> abs (sum crnrVectByVct2)
            | false, false -> (abs (sum crnrVectByVct2)) + (abs (sum crnrVectByVct1))
        (add
            (add crnr (scale (distAxis + v1Count) vct1))
            (scale (distAxis + v2Count) vct2))
    let newDist = distance2D c1 poi
    if newDist <> r1 then failwith "oops"
    poi

let mutable count = 0
let test (c1,r1) (c2,r2) poi =
    printfn "count: %i" count; count <- count + 1
    let expected n = n = 0 || n=1
    let info = distance c1 c2
    let slack1 = r1 - (distance c1 poi)
    let slack2 = r2 - (distance c2 poi)
    if [slack1; slack2; slack1 + slack2] |> List.forall expected
        then poi
        else failwith "oops"

        // bot2 just for early testing
let getPoi bot1 bot2 (corner:Loc) ((_, plane,vct1, vct2):ProjectInfo) =
    let ((x,y,z), r1) = bot1
    let (xc,yc,zc) = corner
    let poi = 
        match plane with
        | XY -> 
            let rPlane = r1 - abs (z - zc)
            let x',y' = getPoi2D ((x,y),rPlane) (xc,yc) vct1 vct2
            (x',y',zc)
        | YZ -> 
            let rPlane = r1 - abs (x - xc)
            let y',z' = getPoi2D ((y,z),rPlane) (yc,zc) vct1 vct2
            (xc,y',z')
        | XZ -> 
            let rPlane = r1 - abs (y - yc)
            let x',z' = getPoi2D ((x,z),rPlane) (xc,zc) vct1 vct2
            (x',yc,z')
    test bot1 bot2 poi
    
let getPoiFromCorner bot1 bot2 ((corner,_), neighbourInfos) =
    neighbourInfos
    |> List.map (fun neighbourInfo -> getPoi bot1 bot2 corner neighbourInfo)

let getPoisForBots (bot1, bot2) =
    //if bot1 = bot2 then [] else
    let (c1, r1), (c2, r2) = bot1, bot2
    //if r2 > r1 then [] else
    let slack = -1 + r1 + r2 - distance c1 c2
    if slack <= 0 then [] else

    let insideCorners = getInsideCornders bot1 bot2
    let crnProjectInfos =
        insideCorners
        |> List.map (fun cnr -> cnr, chooseNeighbours cnr insideCorners)
    let pois= 
        crnProjectInfos
        |> List.collect (getPoiFromCorner bot1 bot2)
        //|> List.map (test bot1 bot2)
    pois

let adjacent (x,y,z) =
    [(x-1,y-1,z-1); (x,y-1,z-1); (x+1,y-1,z-1); (x-1,y,z-1); (x,y,z-1); (x+1,y,z-1); (x-1,y+1,z-1); (x,y+1,z-1); (x+1,y+1,z-1); 
        (x-1,y-1,z); (x,y-1,z); (x+1,y-1,z); (x-1,y,z); (x,y,z); (x+1,y,z); (x-1,y+1,z); (x,y+1,z); (x+1,y+1,z); 
        (x-1,y-1,z-1); (x,y-1,z-1); (x+1,y-1,z-1); (x-1,y,z-1); (x,y,z-1); (x+1,y,z-1); (x-1,y+1,z-1); (x,y+1,z-1); (x+1,y+1,z-1); ]

// rising to the:
// SW, NW, NE, SE
// from top corner      range (bottom) range
// SW: x + y + (z+r)    z,z+r   z-r    z-r,z
// NW: x - y + (z+r)    z,z+r   z-r    z-r,z
// NE: x + y - (z+r)    z,z+r   z-r    z-r,z
// SE: x - y - (z+r)    z,z+r   z-r    z-r,z

type Face = SW | NW | NE | SE

let getXCrosses bots  =
    (([],[],[],[]), bots)
    ||> Seq.fold(fun (sw,nw,ne,se) bot ->
        let ((x,y,z),r) = bot
        (   (bot, SW, x+y+(z+r), (z,z+r))::(bot, SW, x+y+(z-r), (z-r,z))::sw
            , (bot, NW, x-y+(z+r), (z,z+r))::(bot, NW, x-y+(z-r), (z-r,z))::nw
            , (bot, NE, x+y-(z+r), (z,z+r))::(bot, NE, x+y-(z-r), (z-r,z))::ne
            , (bot, SE, x-y-(z+r), (z,z+r))::(bot, SE, x-y-(z-r), (z-r,z))::se))

let checkDuplicateXCrosses botCount (a, b, c, d) =
    let count = List.length a + List.length b + List.length c + List.length d
    if count <> (8 * botCount) then failwith "duplicate crosses"


let nearestIntersection ((bot1, face1, x1, (lowZ1, highZ1)), (bot2, face2, x2, (lowZ2, highZ2))) =
    if highZ1 < lowZ2 || highZ2 < lowZ1 then None else
    let lowZ = max lowZ1 lowZ2
    let highZ = min highZ1 highZ2
    let z =
        match lowZ * highZ with
        //| n when n < 1 -> 0
        | _ -> if abs lowZ < abs highZ then lowZ else highZ
    let locOpt =
        match face1, face2 with
        | SW, NW -> 
            let twoX = x1 + x2 - (2*z)
            let x = twoX - (twoX / 2)
            let twoY = x1 - x2
            let y = twoY - (twoY / 2)
            Some (x,y,z)
        | _ -> None
    match locOpt with
    | Some loc -> test bot1 bot2 loc |> ignore
    | None -> ()
    locOpt

let listIntersections (list1, list2) : seq<Loc>=
    seq{for intx1 in list1 do for intx2 in list2 do yield (intx1, intx2)}
    |> Seq.choose nearestIntersection

let getAllIntersections (sw, nw, ne, se) =
    Seq.collect listIntersections [
        sw, nw;
        sw, ne;        
        sw, se;
        nw, ne;
        nw, se;
        ne, se; ]

let addAll bot =  
    let ((x,y,z), r) = bot
    [(x+r)+(y+z), -1, bot; (x-r)+(y+z), 1, bot]
let addX bot = 
    let ((x,y,z), r) = bot
    [(x+r)-(y+z), -1, bot; (x-r)-(y+z), 1, bot]
let addY bot = 
    let ((x,y,z), r) = bot
    [(y+r)-(x+z), -1, bot; (y-r)-(x+z), 1, bot]
let addZ bot = 
    let ((x,y,z), r) = bot
    [(z+r)-(x+y), -1, bot; (z-r)-(x+y), 1, bot]


let getOverlapInfo bots projection =
    bots
    |> List.collect projection
    |> List.sortBy (fun (n,_,_) -> n)
    |> List.mapFold
        (fun (count, bots:Set<Bot>) (int, inc, bot) ->             
            match inc with
            | 1 -> 
                let count' = count + 1
                let bots' = bots.Add bot
                ((count', int, bots'), (count', bots'))
            | -1 -> 
                let count' = count - 1
                let bots' = bots.Remove bot
                ((count', int, bots'), (count', bots')))
       (0, Set.empty)
    |> fst     


let getDeepest bots projection = 
    getOverlapInfo bots projection
    |> List.maxBy (fun (count,_,_) -> count)
    |> fun (_,_,set) -> set

let Part2 result1 (input : string) = // "result2" 
    let overlaps (c1,r1) (c2,r2) =
        (distance c1 c2) <  r1 + r2    
    let overlapping bots bot =
        bots
        |> List.filter (fun b -> overlaps b bot)
    let overlapCount bots bot =        
        overlapping bots bot
        |> List.length

    let bots = 
        input |> toLines |> List.map parseLine
 
    let heartAll = getDeepest bots addAll
    let heartX = getDeepest bots addX
    let heartY = getDeepest bots addY
    let heartZ = getDeepest bots addZ
    
    let hearts = 
        Set.intersectMany [heartAll; heartX; heartY; heartZ]
        |> List.ofSeq


    let closed =
        hearts
        |> List.filter (fun h -> overlapCount hearts h > 966)
    
    //getOverlapInfo closed addZ
    //|> Seq.iter (fun (count,intr,_) -> printfn "%i, %i" count intr)

    //rangeCount bots (18374823,41219168,34232284)

    seq{
        for x in [18374816..18374829] do   
            for y in [41219161..41219175] do 
                for z in [34232278..34232291] do yield (x,y,z)}
    |> Seq.map (fun loc -> loc, rangeCount bots loc)
    |> Seq.maxBy snd
    |> fst
    |> (distance (0,0,0))
    ////cand
    ////|> List.map (fun c -> overlapCount cand c)
    ////|> List.max

    //let minX = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> x-r)
    //    |> Seq.max
    //let maxX = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> x+r)
    //    |> Seq.min
    //let minY = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> y-r)
    //    |> Seq.max
    //let maxY = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> y+r)
    //    |> Seq.min
    //let minZ = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> z-r)
    //    |> Seq.max
    //let maxZ = 
    //    cand 
    //    |> Seq.map (fun ((x,y,z),r) -> z+r)
    //    |> Seq.min
    
    //print ((minX,maxX), (minY,maxY), (minZ,maxZ))

    //let half n n' = (n + n')/2
    //let x = half minX maxX
    //let y = half minY maxY
    //let z = half minZ maxZ

    //print (x,y,z)

    //rangeCount bots (x,y,z)