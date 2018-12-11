(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day11

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
let NL = System.Environment.NewLine
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

let power serial (x,y) =
     let rack = x + 10
     let big = ((rack * y) + serial) * rack
     ((big % 1000) / 100) - 5

let squares (gx, gy) (sx, sy) =
    seq{ for x in [1..(gx - (sx-1))] do  
            for y in [1..(gy - (sy-1))] do
                yield 
                    [y..(y+(sy-1))] |> List.map (fun y -> [x..(x+(sx-1))] |> List.map (fun x -> (x,y))) }

let squarePower serial (square : (int * int) list list) =
    square
    |> List.collect id
    |> List.sumBy (power serial)
    

let Part1 (input : string) =  
    let serial = input |> int
    let squares = squares (300,300) (3,3)
    
    let serial = 9810

    squares
    |> Seq.map (fun square -> (square.Head.Head, squarePower serial square))
    |> Seq.maxBy snd
    |> fst

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine




//*)