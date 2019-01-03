(* a.cbf.pub/tx/4_EzmwmxDrq1WQr9tCBSUoIimFjAYf92w0qUi2-NNTc/data.html *)

module Day22

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

(* ================ Part A ================ *) 

type Location = int*int
type Type = Rocky | Narrow | Wet
type Times = {Torch: int; Gear: int; Neither: int}
type Region = {
    Location: Location; 
    Type: Type; 
    Erosion: int; 
    Times: Times}
type Cave = { Map: Region[,]; MaxX: int; MaxY:int}

let noRoute = Int32.MaxValue - 8
let defaultTimes = {Torch = noRoute; Gear = noRoute; Neither = noRoute}
let mouthTimes = {defaultTimes with Torch = 0; Gear = 7}
let nonRegion = {
    Location= (-1, -1); 
    Type= Rocky; 
    Erosion= -1; 
    Times= defaultTimes}

let parse text  = 
    let lines = text |> toLines
    let depth = rxMatch "\d+" lines.[0] |> (fun m -> m.Value |> int)
    let (x,y) = rxMatch "(\d+),(\d+)" lines.[1] |> (fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2)
    (depth, (x, y))

let display tLoc (cave:Cave) =
    let symbol {Type=typ} = 
        match typ with
        | Rocky -> '.'
        | Wet -> '='
        | Narrow -> '|'
    Array.init (cave.MaxY+1)  (fun  y -> 
        Array.init (cave.MaxX+1) (fun x -> 
            cave.Map.[x,y] |> function
                | {Location = (0,0)} -> 'M'
                | {Location = loc} when loc = tLoc-> 'T'
                | r -> symbol r))
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
            cave.Map.[x-1,y].Erosion * cave.Map.[x,y-1].Erosion
        (geoIndex + depth) % 20183
    {Location = loc; 
     Type = (getType erosion); 
     Erosion = erosion; 
     Times = defaultTimes}

let enumCoords (grid: 'a[,]) = 
    let (X,Y) = grid.GetUpperBound(0), grid.GetUpperBound(1)
    seq{for y in 0..Y do for x in 0..X do yield (x,y)}

let buildCave depth maxes tLoc : Cave =
    let (X,Y) = maxes
    let map = Array2D.create (X+1) (Y+1) nonRegion
    let cave = {Map = map;  MaxX = X; MaxY = Y}
    enumCoords map
    |> Seq.iter (fun (x,y) ->
        map.[x,y] <- (newRegion depth tLoc cave (x,y)))
    map.[0,0] <- {map.[0,0] with Times = mouthTimes}
    cave

let assessRisk cave =
    cave.Map
    |> Seq.cast<Region>
    |> Seq.sumBy (fun {Type=typ} -> 
        match typ with Rocky -> 0 | Wet -> 1 | Narrow -> 2)

let Part1 (input : string) =
    let depth, targetLoc = parse input    
    buildCave depth targetLoc targetLoc
    //|> display targetLoc
    |> assessRisk

(* ================ Part B ================ *)

type Dirty = { Map: bool[,]; MaxX: int; MaxY:int}

let buildDirtyMap cave : Dirty =
    let lenX, lenY = (Array2D.length1 cave), (Array2D.length2 cave)
    let map = Array2D.create lenX lenY false
    map.[1,0] <- true; map.[1,1] <- true; map.[0,1] <- true
    {Map = map; MaxX = lenX-1; MaxY = lenY-1}

let adjacentTimes (cave: Cave) (x,y) =
    let map = cave.Map
    seq{
        if x > 0 then yield map.[x-1,y].Times
        if y > 0 then yield map.[x,y-1].Times
        if x < cave.MaxX then yield map.[x+1,y].Times
        if y < cave.MaxY then yield map.[x,y+1].Times }

let markAdjacentDirty (dirty: Dirty) (x,y) =
    let map = dirty.Map
    if x > 0 then map.[x-1,y] <- true
    if y > 0 then map.[x,y-1] <- true
    if x < dirty.MaxX then map.[x+1,y] <- true
    if y < dirty.MaxY then map.[x,y+1] <- true 

let compare thisType this adj = 
    let min3 x y z = min (min x y) z
    match thisType with
    | Rocky ->  { 
        Torch = min3 this.Torch (adj.Torch + 1) (adj.Gear + 8)
        Gear = min3 this.Gear (adj.Gear + 1) (adj.Torch + 8)
        Neither = noRoute}
    | Wet ->  { 
        Gear = min3 this.Gear (adj.Gear + 1) (adj.Neither + 8)
        Neither = min3 this.Neither (adj.Neither + 1) (adj.Gear + 8)
        Torch = noRoute}
    | Narrow -> {
        Torch = min3 this.Torch (adj.Torch + 1) (adj.Neither + 8)
        Neither = min3 this.Neither (adj.Neither + 1) (adj.Torch + 8)
        Gear = noRoute}

let compareAdjacent (cave: Cave) (dirty: Dirty) (x,y) =
    dirty.Map.[x,y] <- false
    let region = cave.Map.[x,y]
    let adjacents = adjacentTimes cave (x,y)
    let updated = 
        (region.Times, adjacents)
        ||> Seq.fold (compare region.Type)
    let didImprove = updated <> region.Times
    if didImprove then 
        cave.Map.[x,y] <- {region with Times = updated}
        markAdjacentDirty dirty (x,y)
    didImprove

let explore (cave : Cave) =
    let dirtyMap = buildDirtyMap cave.Map
    let rec improve () =       
        let improvedCount = 
            enumCoords dirtyMap.Map
            |> Seq.filter (fun (x,y) -> 
                dirtyMap.Map.[x,y] &&
                compareAdjacent cave dirtyMap (x,y))
            |> Seq.length
        //printfn "Improved Regions: %i" improvedCount
        if improvedCount = 0 then ()
        else improve ()
    improve ()

let Part2 result1 (input : string) =
    let depth, target = parse input        
    let (targX, targY) = target

    let smallCave = buildCave depth (targX, targY) target
    explore smallCave
    let sampleTime = smallCave.Map.[targX,targY].Times.Torch
    let padding = (sampleTime - (targX + targY)) / 2
    let maxes = targX+padding, targY+padding

    ////smallest that works with my input
    //let maxes = targX+38, targY+0  

    let cave = buildCave depth maxes target
    explore cave
    cave.Map.[targX,targY].Times.Torch
