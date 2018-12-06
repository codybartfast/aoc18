(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day06

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let rec repeat item = seq{ yield item; yield! repeat item }
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

let parseLine  = rxMatch "(\d+), (\d+)" >> fun mtch ->
    let grp idx = groupValue mtch idx
    let grpi = grp >> int
    (grpi 1, grpi 2)
   
let ring (X, Y) radius =
    let steps = radius - 1 
    seq{ 
        for xDiff in [0..steps] do 
            let yDiff = steps - xDiff
            yield (X + xDiff, Y + yDiff)
            yield (X + xDiff, Y - yDiff)
            yield (X - xDiff, Y + yDiff)
            yield (X - xDiff, Y - yDiff) }
            |> Seq.distinct
    //let (X, Y) = (x - radius, y - radius)
    //let length = (2 * radius) + 1
    //let steps = length - 1
    //let top = seq {for x in [X..(X + steps)] do yield (x, Y)}
    //let right = seq {for y in [Y..(Y + steps)] do yield (X + steps, y)}
    //let bottom = seq {for x in [X..(X + steps)] do yield (x, Y + steps)}
    //let left = seq {for y in [Y..(Y + steps)] do yield (X, y)}
    //Seq.concat [top; right; bottom; left]

let rings centre =
    let rec rings radius =
        seq{ 
            yield ring centre radius
            yield! rings (radius + 1) }
    rings 1

let inline findNearest (points : Set<int*int>) point =
    let rec find (rings : seq<seq<int*int>>) =       
        let ring = Seq.head rings
        let neighbours = ring |> Seq.filter (fun p -> points.Contains p) |> List.ofSeq
        match neighbours.Length with
            | 1 -> Some ( neighbours |> Seq.head)
            | 0 -> find (Seq.tail rings)
            | _ -> None
    let rings = rings point
    find rings


let Part1 (input : string) = 
    let points =  
        input |> toLines |> List.map parseLine
        |> Set

    let (X, Y) = (points |> Seq.maxBy fst |> fst, points |> Seq.maxBy snd |> snd)

    let perimiter = 
        seq{
            yield! seq{for x in 0..X do yield (x, 0)}
            yield! seq{for x in 0..X do yield (x, Y)}
            yield! seq{for y in 0..Y do yield (0, y)}
            yield! seq{for y in 0..Y do yield (X, y)}}
    
    let infinite =
        perimiter
        |> Seq.map (findNearest points)
        |> Set

    seq{for x in 0..X do 
        (print x) |> ignore
        for y in 0..Y do yield (x, y)}
    |> Seq.map (findNearest points)
    |> Seq.filter(fun p -> not (infinite.Contains p))
    |> Seq.choose id
    |> Seq.countBy id
    |> Seq.maxBy snd

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =   "result2" (*
    input |> toLines



//*)