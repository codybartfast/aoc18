(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day14

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
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
let toHex = 
    BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

type Board = ((int[] * int) * (int * int))

let getBoard improveCount =
    let recipies = Array.init (improveCount + 11) (fun i -> -1)
    recipies.[0] <- 3
    recipies.[1] <- 7
    ((recipies, 2), (0,1))

let getDigits quality =
    match quality / 10 with
    | 0 -> [|quality|]
    | 1 -> [|1; quality % 10|]
    | _ -> failwith "oops"

let createRecipes (board : Board) = 
    let ((recipies, size), (elf1, elf2)) = board
    let (quality1, quality2) = recipies.[elf1], recipies.[elf2]
    let digits  = getDigits (quality1 + quality2)

    recipies.[size] <- digits.[0]
    let newSize =
        match digits.Length with
        | 1 -> size + 1
        | 2 -> recipies.[size + 1] <- digits.[1]; size + 2
        | _ -> failwith "oops"
    
    let moveElf elf = (1 + (elf + recipies.[elf])) % newSize
    ((recipies, newSize), (moveElf elf1, moveElf elf2))

let rec createRecipesTo targetSize (board : Board) =
    let ((_, size), _) = board
    match size < targetSize with
    | true -> createRecipesTo targetSize (createRecipes board)
    | false -> board

let Part1 (input : string) =  // "result1" (*
     let threshold = int input        
     //let threshold = 2018
     let board = createRecipesTo (threshold + 10) (getBoard threshold)
     let ((recipes, _), _)  = board
     let ten = recipes.[threshold..(threshold+9)]
     ten |> Seq.map(fun i -> i.ToString()) |> String.concat ""



//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)