(* a.cbf.pub/tx/I9FwEdzf5qLLCpRjgZxlKAzgyvsKJN-H407f9efaaNg/data.html *)

module Day10

open System
open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let NL = System.Environment.NewLine
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

(* ================ Part A ================ *) 

let parseLine  = 
    rxMatch "(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" >> fun mtch ->
    let grp idx = groupValue mtch idx
    let grpi = grp >> int
    (grpi 1, grpi 2), (grpi 3, grpi 4) 

let displayLine points =
    let xs = points |> List.map fst |> Set
    [0..(xs.MaximumElement)]
    |> Seq.map (fun x -> 
        if xs.Contains x then '#' else ' ')
    |> toString
 
let display lights =
    let shiftToOrigin coords =
        let xMin = coords |> List.groupBy fst |> List.minBy fst |> fst
        let yMin = coords |> List.groupBy snd |> List.minBy fst |> fst
        coords |> List.map (fun (x,y) -> (x - xMin, y - yMin))
    let toRows = List.groupBy snd >> List.sortBy fst >> List.map snd
    lights
    |> (shiftToOrigin >> toRows)
    |> Seq.map displayLine 
    |> String.concat NL
 
let lightSequence lights vectors =
    (lights, Seq.initInfinite id)
    ||> Seq.scan (fun lights time -> 
        (lights, vectors) 
        ||>  List.map2 (fun (lx, ly) (vx, vy) -> (lx + vx), (ly + vy)))
 
let size (_time, lights) =
    let xs = lights |> List.map fst |> List.sort
    let ys = lights |> List.map snd |> List.sort
    (List.last xs - List.head xs) + (List.last ys - List.head ys)

let rec findSmallest timedSequenceLights =
    timedSequenceLights
    |> Seq.pairwise
    |> Seq.map (fun (tLights1, tLights2) -> 
        ((size tLights1 - size tLights2), tLights1))
    |> Seq.skipWhile (fun (reduction, _) -> reduction > 0)
    |> Seq.head |> snd

let findMessage input =
    let data = input |> toLines |> List.map parseLine
    let lights = data |> List.map fst
    let vectors = data |> List.map snd
    let lightSequence = lightSequence lights vectors
    let timedSequence = 
        lightSequence |> Seq.mapi (fun i lights -> (i, lights))
    findSmallest timedSequence

let Part1 (input : string) =
    findMessage input
    |> snd |> display |> (fun s -> NL + s + NL)

(* ================ Part B ================ *)

let Part2 result1 (input : string) =
    findMessage input
    |> fst
