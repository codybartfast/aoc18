(* a.cbf.pub/tx/Wrzexl1JdBwGDCBPF6BhE97IisTZJNM3W2uDEi6heIo/data.html *)

module Day18

open System

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

(* ================ Part A ================ *) 

type Acre = Trees | Lumber | Open
type Area = Acre[,]

let parseLine  = 
    toChars
    >> Array.map (function
        | '.' -> Open
        | '#' -> Lumber
        | '|' -> Trees 
        | _ -> failwith "oops")

let to2D rows =
    let Y = Array.length rows
    let X = Array.length rows.[0]
    Array2D.init X Y (fun x y -> rows.[y].[x])

let parseInput =
    toLines >> List.toArray >> Array.map parseLine >> to2D

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

let get (area:Area) (x,y) = 
    if x >= area.GetLowerBound(0) 
        && x <= area.GetUpperBound(0)
        && y >= area.GetLowerBound(1)
        && y <= area.GetUpperBound(1)
    then Some area.[x,y] else None

let getAdjacent area (x,y) =
    seq{
        yield (x-1,y-1); yield (x,y-1); yield (x+1,y-1); 
        yield (x-1,y);                  yield (x+1,y); 
        yield (x-1,y+1); yield (x,y+1); yield (x+1,y+1); }
    |> Seq.choose (get area)

let strangeMagic acre adjacent =
    let count (acre:Acre) =
        adjacent |> Seq.filter (fun a -> a = acre) |> Seq.length
    match acre with
    | Open when count Trees >= 3 -> Trees
    | Open -> Open
    | Trees when count Lumber >= 3 -> Lumber
    | Trees -> Trees
    | Lumber when count Lumber >= 1 && count Trees >= 1 -> Lumber
    | Lumber -> Open

let minutePasses area =
    Array2D.init 
            (Array2D.length1 area)
            (Array2D.length2 area)
            (fun x y -> strangeMagic area.[x,y] (getAdjacent area (x,y)))

let coords (area:Area) =
    seq{for y in seq{area.GetLowerBound(1)..area.GetUpperBound(1)} do
        for x in seq{area.GetLowerBound(0)..area.GetUpperBound(0)} do
        yield (x,y)}   
            
let resourceValue area =
    let (_, tr, lm) =
        ((0,0,0), coords area)
        ||> Seq.fold(fun (op,tr,lm) (x,y) ->
            match area.[x,y] with
            | Open -> (op+1, tr, lm)
            | Trees -> (op, tr+1, lm)
            | Lumber -> (op, tr, lm+1))
    tr * lm

let areas = 
    Seq.unfold (fun area ->
        let newArea = minutePasses area
        Some (area, newArea))      
        
let Part1 (input : string) =  
    areas (parseInput input) |> Seq.item 10 |> resourceValue

(* ================ Part B ================ *)

let Part2 result1 (input : string) = 

    let knownAreas, prevTime, repeatedTime =
        ((Map.empty, 0, None), areas (parseInput input))
        ||> Seq.scan(fun (knownAreas, time, _) area ->
            let repeatedTimes = 
                match knownAreas.TryGetValue area with
                | true, prevTime -> Some (prevTime, time)
                | _ -> None
            (knownAreas.Add (area, time), time + 1, repeatedTimes))
        |> Seq.pick (fun (known, _, repeatedTimes) ->
            match repeatedTimes with
            | None -> None
            | Some (prev, repeat) -> Some (known, prev, repeat))
    
    let period = repeatedTime - prevTime
    let remaining =  1_000_000_000 - repeatedTime
    let offset = remaining % period
    let equivalentTime = prevTime + offset

    knownAreas 
        |> Map.toSeq 
        |> Seq.find (fun (_, time) -> time = equivalentTime)
        |> fst
        |> display
        |> resourceValue
    