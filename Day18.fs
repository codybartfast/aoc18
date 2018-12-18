(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day18

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
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) |> List.ofArray
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

type Acre = Trees | Lumber | Open
type Area = Acre[,]
(* ================ Part A ================ *) 

let parseLine  = 
    // .#.#...|#.
    toChars
    >> Array.map (function
        | '.' -> Open
        | '#' -> Lumber
        | '|' -> Trees 
        | _ -> failwith "oops")

let display (area:Area) =
    let symbol = function   
        | Lumber -> '#'
        | Open -> '.'
        | Trees -> '|'
    let diplayLine line = seq{    
            yield! seq{ 
                for x in seq{area.GetLowerBound(0)..area.GetUpperBound(0)}
                    do yield symbol area.[x,line]}
            yield '\n'}
    let symbols = 
        seq{area.GetLowerBound(1)..area.GetUpperBound(1)}
        |> Seq.collect diplayLine
    printfn "%s" (toString symbols)
    area

let to2D rows =
    let Y = Array.length rows
    let X = Array.length rows.[0]
    Array2D.init X Y (fun x y -> rows.[y].[x])

let get (area:Area) (x,y) = 
    if x >= area.GetLowerBound(0) 
        && x <= area.GetUpperBound(0)
        && y >= area.GetLowerBound(1)
        && y <= area.GetUpperBound(1)
    then Some area.[x,y] else None

let getAdjacent (area:Area) (x,y) =
    [|  (x-1,y-1); (x,y-1); (x+1,y-1); 
        (x-1,y);            (x+1,y); 
        (x-1,y+1); (x,y+1); (x+1,y+1); |]
    |> Array.choose (get area)

let strangeMagic (acre:Acre) (adjacent : Acre[])= 
    let counts = 
        adjacent 
        |> Array.countBy id 
        //|> Array.map (fun (list, count) -> list.Head, count)
        |> Map
    let count (acre:Acre) = 
        if counts.ContainsKey acre then counts.[acre] else 0
    match acre with
    | Open when count Trees >= 3 -> Trees
    | Open -> Open
    | Trees when count Lumber >= 3 -> Lumber
    | Trees -> Trees
    | Lumber when count Lumber >= 1 && count Trees >= 1 -> Lumber
    | Lumber -> Open

let minutePasses (area:Area) =
    Array2D.init 
            (Array2D.length1 area)
            (Array2D.length2 area)
            (fun x y -> strangeMagic area.[x,y] (getAdjacent area (x,y)))
            
let resourceValue (area :Area) =
    let coords = seq{for y in seq{area.GetLowerBound(1)..area.GetUpperBound(1)} do
                      for x in seq{area.GetLowerBound(0)..area.GetUpperBound(0)} do
                        yield (x,y)}
    ((0,0,0), coords)
    ||> Seq.fold(fun (op,tr,lm) (x,y) ->
        match area.[x,y] with
        | Open -> (op+1,tr,lm)
        | Trees -> (op,tr+1,lm)
        | Lumber -> (op,tr,lm+1))
    
        
let Part1 (input : string) =  // "result1" (*
    let area =
        input |> toLines |> List.toArray |> Array.map parseLine |> to2D


    let areas = 
        area 
        |> Seq.unfold (fun area ->
            let newArea = minutePasses area
            Some (area, newArea))

    let tenth = Seq.item 10 areas
    let (op,tr,lm) = resourceValue tenth
    tr * lm
//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)