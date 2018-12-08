(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day08

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

let readMetadata (data, mdTotal) mdCount =
    (List.skip mdCount data)
    , mdTotal + (List.take mdCount data |> List.sum)

let rec readChild (data, mdTotal) =
    match data with
    | [] -> ([], mdTotal)
    | [_] -> failwith "oops"
    | chCount::(mdCount::tail) -> 
        let (data, mdTotal) =
            ((tail, mdTotal), [1..chCount])
            ||> List.fold (fun state _ -> readChild state)
        readMetadata (data, mdTotal) mdCount



    
let Part1 (input : string) =  //  "result1" (*
    let data = input.Split(' ') |> List.ofArray |> List.map (int)
    readChild (data, 0)
    




//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =   "result2" (*
    input |> toLines



//*)