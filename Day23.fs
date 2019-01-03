(* a.cbf.pub/tx/55qPliZqaE6ikko7I26zzf2a6EQHfrP4Z43bR18khwo/data.html *)

module Day23

# nowarn "25"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

(* ================ Part A ================ *) 

type Bot = ((int*int*int)*int)

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
    let strongCntr, radius = strongest
    let inRange = bots |> List.filter (fun bot -> 
        let botCentre = fst bot
        let dist = distance strongCntr botCentre 
        dist <= radius)
    inRange |> List.length

(* ================ Part B ================ *)

let rangeCount bots loc =
    bots 
    |> Seq.filter (fun (cntr, rad) -> rad >= distance loc cntr)
    |> Seq.length

let overlaps (c1,r1) (c2,r2) =
    (distance c1 c2) <  r1 + r2    

let overlapping bots bot =
    bots
    |> List.filter (fun other -> overlaps other bot)

let overlapCount bots bot =        
    overlapping bots bot
    |> List.length

let getMinimumViableBots bots =
    // Finds the smallest collection of bots where the total number of bots
    // is greater than the number of bots that are overlapping.
    // Not sure what we can say for certain about this group, but it
    // happens, perhaps by luck, perhabs by kind puzzle designers, to
    // contain all the bots we're looking for and just a few we aren't.
    bots
    |> List.groupBy (overlapCount bots)    
    |> List.sortByDescending fst
    |> List.mapFold (fun (totalBots, (accumulatedBots : Bot list list)) (overlapCount, bots) ->
            let newTotal = totalBots + bots.Length
            let accumulatedBots = bots::accumulatedBots
            let state = newTotal, accumulatedBots
            let result = overlapCount, newTotal, accumulatedBots
            result, state)
        (0, [])
    |> fst
    |> List.find (fun (olapCount, botCount, _botLists) ->
        botCount >= olapCount)
    |> (fun (_, _, botLists) -> botLists)
    |> List.collect id

let rec mostOveralapping (bots : Bot list) =
    // Remove the least well connect bots recursively until
    // all the remaining have the same number of overlaps.
    // Not sure this would always gibe us the bots we need.
    let byOverlapCount = 
        bots
        |> List.groupBy (overlapCount bots)    
        |> List.sortBy fst
    match byOverlapCount with
    | [_,bots] -> bots
    | _::tail -> 
        tail
        |> List.collect (fun (_,bots) -> bots)
        |> mostOveralapping

let projectionLimits bots projection =
    let mn, mx = Int32.MinValue, Int32.MaxValue
    ((mn, mx), bots)
    ||> List.fold (fun (mn, mx) bot ->
        let high, low = projection bot
        max mn low, min mx high)

let addAll ((x,y,z), r) = (x+r)+(y+z), (x-r)+(y+z)
let addX ((x,y,z), r) = (x+r)-(y+z), (x-r)-(y+z)
let addY ((x,y,z), r) = (y+r)-(x+z), (y-r)-(x+z)
let addZ ((x,y,z), r) = (z+r)-(x+y), (z-r)-(x+y)

let Part2 result1 (input : string) = 
    let bots = input |> toLines |> List.map parseLine
 
    // largest collection of mutually overlapping bots
    let clique = 
        bots
        |> getMinimumViableBots
        |> mostOveralapping
   
    // bot ranges are 'diamond' shaped octahedrons. Here we extend the
    // surfaces onto different axis and find values that are common to all
    // bots.
    let (minAllProj, maxAllProj) = projectionLimits clique addAll
    let (minXProj, maxXProj) = projectionLimits clique addX
    let (minYProj, maxYProj) = projectionLimits clique addY
    let (minZProj, maxZProj) = projectionLimits clique addZ
    
    // And work out the actual values of x, y, and z that will give us a
    // 'box' that contains the coordinates we're looking for
    //
    // (x+y+z) + (+x-y-z) = 2x
    let (minX, maxX) = (minAllProj + minXProj) / 2, (maxAllProj + maxXProj) / 2
    // (x+y+z) + (-x+y-z) = 2y
    let (minY, maxY) = (minAllProj + minYProj) / 2, (maxAllProj + maxYProj) / 2
    // (x+y+z) + (-x-y+z) = 2z
    let (minZ, maxZ) = (minAllProj + minZProj) / 2, (maxAllProj + maxZProj) / 2
    
    // We have a region small enough to do a naive search for the closest point
    // (perhaps a little too naive)
    seq{for x in [minX..maxX] do   
            for y in [minY..maxY] do 
                for z in [minZ..maxZ] do yield (x,y,z)}
    |> Seq.map (fun loc -> loc, rangeCount bots loc)
    |> Seq.maxBy snd
    |> fst
    |> (distance (0,0,0))
