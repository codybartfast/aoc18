(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day05

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Numerics

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

let rec mark i (chars:char[]) =
    let asciiVal indx = chars.[indx] |> int
    match i with 
    | i when i >= (chars.Length - 2) -> chars
    | _ ->
        if Math.Abs(asciiVal i - asciiVal (i + 1)) = 32 
        then chars.[i] <- '_'; chars.[i + 1] <- '_'; mark (i + 1) chars
        else  mark (i + 1) chars

let rec collapse (polymer:string) =
    polymer
    |> toChars
    |> mark 0
    |> toString
    |> (fun str -> str.Replace("_", ""))
    |> (fun newPoly ->
        if len newPoly = len polymer 
        then len newPoly
        else collapse newPoly)

    
let Part1 input = 
    input |> collapse


(* ================ Part B ================ *)

let Part2 result1 input = 
    "result2"
