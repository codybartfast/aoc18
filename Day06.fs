(* a.cbf.pub/tx/iD56U65nlMdpnJXZY7SoYT5ERY1yXXbDWcsg_9TYgls/data.html *)

module Day06

open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

(* ================ Part A ================ *) 

let parseLine  = rxMatch "(\d+), (\d+)" >> fun mtch ->
    let grpi = (groupValue mtch) >> int
    (grpi 1, grpi 2)

let getSize locations =     
        (locations |> Seq.maxBy fst |> fst
        , locations |> Seq.maxBy snd |> snd)

let allCoords (xMax, yMax) = 
    seq{for x in 0..xMax do 
         for y in 0..yMax do yield (x, y)}

let distance (x1, y1) (x2, y2) =  (abs (x1 - x2)) + (abs (y1 - y2))
  
let getNearest locations location =
    let nearest = 
        locations
        |> Seq.groupBy (distance location)
        |> Seq.minBy fst
        |> snd
        |> List.ofSeq
    match nearest.Length with
    | 1 -> Some nearest.Head
    | _ -> None 

let Part1 (input : string) = 
    let locations =  
        input |> toLines |> List.map parseLine
        |> Set

    let (xMax, yMax) = getSize locations

    let perimiter = seq{
            yield! seq{for x in 0..xMax do yield (x, 0)}
            yield! seq{for x in 0..xMax do yield (x, yMax)}
            yield! seq{for y in 0..yMax do yield (0, y)}
            yield! seq{for y in 0..yMax do yield (xMax, y)}}
    
    let infiniteLocations =
        perimiter
        |> Seq.map (getNearest locations)
        |> Set

    allCoords (xMax, yMax)
    |> Seq.map (getNearest locations)
    |> Seq.filter(fun p -> not (infiniteLocations.Contains p))
    |> Seq.choose id
    |> Seq.countBy id
    |> Seq.maxBy snd |> snd

(* ================ Part B ================ *)

let maxDistance = 10000

let Part2 result1 (input : string) =
    let locations =  
        input |> toLines |> List.map parseLine
        |> Set

    allCoords (getSize locations)
    |> Seq.map (fun (x, y) -> locations |> Seq.sumBy (distance (x,y)))
    |> Seq.filter (fun dist -> dist < maxDistance)
    |> Seq.length
