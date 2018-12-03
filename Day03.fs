(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day03

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

let parseLine line =
    line
    |> rxMatch "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    |> (fun m -> 
        let grp (i:int) =  m.Groups.[i].Value |> int
        (grp 1), (grp 2, grp 3), ((grp 4), (grp 5)))


let enumCoords (id, (l, t), (w, h)) =
    seq{for x in l .. (l+(w-1)) do 
        for y in t .. (t+(h-1)) do
        yield (x,y)
    }

let Part1 input = 
    input |> toLines
    |> List.map parseLine
    |> List.map enumCoords
    |> Seq.collect id
    |> Seq.countBy id
    |> filterCount (fun (_, count) -> count > 1)
    
    

      
(* ================ Part B ================ *)

let Part2 result1 input = 
    "day3 result2"
