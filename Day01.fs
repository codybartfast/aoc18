module Day01

#nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let print obj = (printfn "%O" obj); obj
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

(* ================ Part A ================ *) 

let PartA (input : string[]) =
    input
    |> Array.map int
    |> Array.fold (+) 0


(* ================ Part B ================ *)

let rec repeat item = seq{ yield item; yield! repeat item }

let applyDelta freq =
    let mutable freq = freq
    (fun delta -> 
        freq <- freq + delta
        freq)

let isDuplicate () =
    let mutable set = Set.empty
    (fun item ->
        let isDup = set.Contains item
        set <- set.Add item
        (item, isDup))

let PartB resultA (input : string[]) = 
 
    let repeatedDeltas =
        input
        |> Array.map int    
        |> repeat
        |> Seq.collect id
    
    let values =
        repeatedDeltas
        |> Seq.map (applyDelta 0)

    values
    |> Seq.map (isDuplicate ())
    |> Seq.find (fun (_, isDup) -> isDup)
    |> fst

    //values |> Seq.item 997
