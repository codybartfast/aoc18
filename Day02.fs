(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day02

#nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Runtime.InteropServices

let lines (text:string) = 
    text.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofSeq 
let rec repeat item = seq{ yield item; yield! repeat item }
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let fromChars (chrs : char[]) = String(chrs)
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

let doubleCount line = 
    line
    |> toChars
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.contains 2

let trippleCount line = 
    line
    |> toChars
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.contains 3

let Part1 input = 
    let lines = input |> lines
    (lines 
    |> Seq.map doubleCount
    |> filterCount id) *
    (lines 
    |> Seq.map trippleCount
    |> filterCount id)

(* ================ Part B ================ *)

let hasOneDiff (id1, id2) = 
    (toChars id1, toChars id2)
    ||> Seq.map2 (fun c1 c2 -> c1 <> c2)
    |> filterCount id
    |> (function | 1 -> Some (id1, id2) | _ -> None)

let rec allPairs list = 
    match list with
    | [] -> []
    | _::[] -> []
    | head::tail -> 
        List.map (fun x -> (head, x)) tail
        @ allPairs tail
    

let Part2 result1 input = 
    input
    |> lines
    |> allPairs
    |> List.choose hasOneDiff

    
