(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day03

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open Day02

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

let parseLine line =
    line
    |> rxMatch "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    |> (fun m -> 
        let grp (i:int) =  m.Groups.[i].Value |> int
        (grp 1), (grp 2, grp 3), ((grp 4), (grp 5)))


let enumCoords (id, (l, t), (w, h)) =
    seq{for x in l .. (l+(w-1)) do 
        for y in t .. (t+(h-1)) do
        yield (id, (x,y))
    }

let Part1 input = 
    input |> toLines
    |> List.map parseLine
    |> List.map enumCoords
    |> Seq.collect id
    |> Seq.countBy id
    |> filterCount (fun (_, count) -> count > 1)
    
    

      
(* ================ Part B ================ *)

let lineOverlaps (s1, f1) (s2, f2) =
    ((s1 >= s2 && s1 <= f2) || (s2 >= s1 && s2 <= f1))

let overlaps (id1, (l1, t1), (w1, h1)) (id2, (l2, t2), (w2, h2)) =
    lineOverlaps (l1, l1 + (w1 - 1)) (l2, l2 + (w2 - 1))
    &&
    lineOverlaps (t1, t1 + (h1 - 1)) (t2, t2 + (h2 - 1))

    
let claimOverlaps claims claim1 =
    let over = 
        claims
        |> Seq.map (fun claim2 ->
             (claim1, (claim1 <> claim2) && overlaps claim1 claim2))
        |> Seq.map snd
        |> Seq.exists id
    (claim1, over)



let Part2 result1 input = 
    let claims = 
        input |> toLines
        |> List.map parseLine
    claims
    |> Seq.map (fun claim -> claimOverlaps claims claim)
    //|> Seq.countBy (fun (_, over) -> over)
    //|> List.ofSeq
    |> Seq.filter (fun (_, over) -> not over)
    |> Seq.exactlyOne
    