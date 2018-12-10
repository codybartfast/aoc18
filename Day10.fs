(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day10

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let rec repeat item = seq{ yield item; yield! repeat item }
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

//position=<-42281, -42228> velocity=< 4,  4>
let parseLine  = rxMatch "\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)" >> fun mtch ->
    let grp idx = groupValue mtch idx
    let grpi = grp >> int
    (grpi 1, grpi 2), (grpi 3, grpi 4) 

let displayLine points : unit =
    let set = points |> List.map fst |> Set
    [0..99]
    |> Seq.iter(fun i -> 
        if set.Contains i then Console.Write("#")
        else Console.Write(" "))
    Console.WriteLine()
        
let display (lights : (int * int) list) =
    let xMin =
        lights 
        |> List.groupBy fst |> List.minBy fst |> fst
    let yMin =
        lights 
        |> List.groupBy snd |> List.minBy fst |> fst
    let relCoords =
        lights |> List.map (fun (x,y) -> (x - xMin, y - yMin))
    let visible =
        relCoords |> List.filter (fun (x, y) -> (x < 100) && (y < 100))
    let rows = 
        visible |> List.groupBy snd |> Map
    [0..30]
    |> List.iter (fun i ->
        if rows.ContainsKey i then displayLine (rows.[i])
        else Console.WriteLine())
 
let lightSequence (lights : (int * int) list) (vectors : (int * int) list) =
    (lights, Seq.initInfinite id)
    ||> Seq.scan (fun lights _ -> 
        (lights, vectors) ||>  List.map2 (fun (lx, ly) (vx, vy) -> (lx + vx), (ly + vy)))
 
let spread lights =
    let xs = lights |> List.map fst |> List.sort
    let ys = lights |> List.map snd |> List.sort
    (List.last xs - List.head xs) + (List.last ys - List.head ys)
    
let Part1 (input : string) =  // "result1" (*
    let data = input |> toLines |> List.map parseLine
    let lights = data |> List.map fst
    let vectors = data |> List.map snd
    let sequence = lightSequence lights vectors
    
    sequence
    |> Seq.skipWhile (fun lights -> (spread lights) > 300)
    |> Seq.iter(fun lights ->
        Console.WriteLine(String('_', 50))
        display lights
        Console.ReadKey() |> ignore)
    

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =   "result2" (*
    input |> toLines |> Seq.map parseLine




//*)