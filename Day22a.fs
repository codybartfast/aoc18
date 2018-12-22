(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day22a

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

(* ================ Part A ================ *) 
type Time = int
type Type = Rocky | Narrow | Wet
type History = { Torch: Time; Gear: Time; Neither: Time}
type Region = {Type : Type; Erosion : int; History: History}
type Cave = Region[,]

let zeroHistory = {Torch = 0; Gear = 0; Neither = 0}
let originHistory = {Torch = 0; Gear = 7; Neither = 0}

let parse text  = 
    let lines = text |> toLines
    let depth = rxMatch "\d+" lines.[0] |> (fun m -> m.Value |> int)
    let (x,y) = rxMatch "(\d+),(\d+)" lines.[1] |> (fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2)
    (depth, (x, y))

let display tLoc (cave:Cave)  =
    let symbol loc {Type=typ} = 
        if loc = (0,0) then 'M' else
        if loc = tLoc then 'T' else
        match typ with
        | Rocky -> '.'
        | Wet -> '='
        | Narrow -> '|'
    let diplayLine line = seq{    
            yield! seq{ 
                for x in seq{cave.GetLowerBound(0)..cave.GetUpperBound(0)}
                    do yield symbol (x,line) cave.[x,line]}
            yield '\n'}
    let symbols = 
        seq{cave.GetLowerBound(1)..cave.GetUpperBound(1)}
        |> Seq.collect diplayLine
    printfn "%s" (toString symbols)

let addHistory isTarget prevHist pType nType =
    if true then
        match (pType, nType) with
        | Rocky, Rocky -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Gear = prevHist.Gear + 1
                                Neither = 0 }
        | Narrow, Rocky -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Gear = prevHist.Neither + 8
                                Neither = 0 }
        | Wet, Rocky -> { prevHist with 
                                Torch = prevHist.Neither + 8
                                Gear = prevHist.Gear + 1
                                Neither = 0 }
        | Rocky, Narrow -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Neither = prevHist.Gear + 8
                                Gear = 0 }
        | Narrow, Narrow -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Neither = prevHist.Neither + 1
                                Gear = 0 }
        | Wet, Narrow -> { prevHist with 
                                Torch = prevHist.Gear + 8
                                Neither = prevHist.Neither + 1
                                Gear = 0 }
        | Rocky, Wet -> { prevHist with 
                                Gear = prevHist.Gear + 1
                                Neither = prevHist.Torch + 8
                                Torch = 0 }
        | Narrow, Wet -> { prevHist with 
                                Gear = prevHist.Torch + 8
                                Neither = prevHist.Neither + 1
                                Torch = 0 }
        | Wet, Wet -> { prevHist with 
                                Gear = prevHist.Gear + 1
                                Neither = prevHist.Neither + 1
                                Torch = 0 }
    else
        match (pType, nType) with
        | Rocky, Rocky -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Gear = prevHist.Gear + 8}//
        | Narrow, Rocky -> { prevHist with 
                                Torch = prevHist.Torch + 1
                                Gear = prevHist.Neither + 8}
        | Wet, Rocky -> { prevHist with 
                                Torch = prevHist.Neither + 8
                                Gear = prevHist.Gear + 8}//

    
let getNext1 isTgt (prev : Region) (next : Region) : History =
    let prevHistory = prev.History
    let prevType, nextType = prev.Type, next.Type
    let nextHistory = addHistory isTgt prevHistory prevType nextType
    nextHistory

let getNext2 isTgt (prev1 : Region) (prev2 : Region) (next : Region) : History =
    let hist1 = getNext1 isTgt prev1 next
    let hist2 = getNext1 isTgt prev2 next
    {   Torch = min hist1.Torch hist2.Torch
        Gear = min hist1.Gear hist2.Gear
        Neither = min hist1.Neither hist2.Neither }

let getNext3 isTgt (prev1 : Region) (prev2 : Region) (prev3 : Region) (next : Region) : History =
    let hist12 = getNext2 isTgt prev1 prev2 next
    let hist3 = getNext1 isTgt prev3 next
    {   Torch = min hist12.Torch hist3.Torch
        Gear = min hist12.Gear hist3.Gear
        Neither = min hist12.Neither hist3.Neither }

let getNext4 isTgt (prev1 : Region) (prev2 : Region) (prev3 : Region) (prev4 : Region) (next : Region) : History =
    let hist12 = getNext2 isTgt prev1 prev2 next
    let hist34 = getNext2 isTgt prev3 prev4 next
    {   Torch = min hist12.Torch hist34.Torch
        Gear = min hist12.Gear hist34.Gear
        Neither = min hist12.Neither hist34.Neither }

let buildCave depth tLoc dims : Cave =
    let getType erosion =
        match erosion % 3 with
        | 0 -> Rocky
        | 1 -> Wet
        | 2 -> Narrow
    let (X,Y) = dims
    let cave = Array2D.create X Y {Type = Rocky; Erosion = 0; History = zeroHistory}
    seq{for y in [0..Y-1] do for x in [0..X-1] do yield (x,y)}
    |> Seq.iter (fun (x,y) ->
        let loc = (x,y)
        let erosion  =
            let geoIndex =
                if loc = (0,0) || loc = tLoc then 0 else
                if y = 0 then x * 16807 else
                if x = 0 then y * 48271 else
                cave.[x-1,y].Erosion * cave.[x,y-1].Erosion
            (geoIndex + depth) % 20183
        let region = {Type = getType erosion; Erosion = erosion; History = zeroHistory}
        cave.[x,y] <- region)
    seq{for y in [0..Y-1] do for x in [0..X-1] do yield (x,y)}
    |> Seq.iter (fun (x,y) ->
        let loc = (x,y)
        let isTgt = loc = tLoc
        let region = cave.[x,y]
        let history =
            if loc = (0,0) then originHistory else
            if y = 0 then getNext1 isTgt cave.[x-1,y] region else
            if x = 0 then getNext1 isTgt cave.[x,y-1] region else
            getNext2 isTgt cave.[x-1,y] cave.[x,y-1] region
        let region = { region with History = history}
        cave.[x,y] <- region)
    cave

let lookAround tLoc (cave:Cave) radius =
    let (tx,ty) = tLoc
    let minX = max 0 (tx-radius)
    let maxX = tx+radius
    let minY = max 0 (ty-radius)
    let maxY = ty+radius

    seq{ for y in minY..maxY do for x in minX..maxX do yield (x,y)}
    |> Seq.iter (fun (x,y) ->
        let loc = (x,y)
        let isTgt = loc = tLoc
        let region = cave.[x,y]      
        let nextHist = 
            match x, y with
            | 0, 0 -> region.History
            | _, 0 -> getNext3 isTgt cave.[x-1,y] cave.[x+1,y] cave.[x,y+1] region
            | 0, _ -> getNext3 isTgt cave.[x,y-1] cave.[x,y+1] cave.[x+1,y] region
            | _ -> getNext4 isTgt cave.[x,y-1] cave.[x,y+1] cave.[x+1,y]  cave.[x-1,y] region
        //printfn "diff = %O" (region.History <> nextHist)
        let region = { region with History = nextHist}
        cave.[x,y] <- region)

let assessRisk (X,Y) (cave:Cave) =
    seq{for y in 0..Y do for x in 0..X do yield (x,y)}
    |> Seq.map (fun (x,y) -> cave.[x,y])
    |> Seq.sumBy (fun {Type=typ} -> 
        match typ with Rocky -> 0 | Wet -> 1 | Narrow -> 2)

let Part1 (input : string) =  // "result1" (*
    let depth, tLoc = 510, (10,10)
    //let depth, tLoc = parse input
    let tMax = max (fst tLoc) (snd tLoc)
    let radius = tMax * 1
    let tx, ty = tLoc
    let cave = buildCave depth tLoc (tx + radius, ty + radius)
    //lookAround tLoc cave (radius-3)
    printfn "risk: %i" (assessRisk (tx,ty) cave)
    [(radius-3)..(-1)..0]
    |> Seq.iter (fun radius -> 
        lookAround tLoc cave (print radius))
    //display tLoc cave
    cave.[tx, ty]
    //"done"
    // display tLoc
    //|> assessRisk targetLoc
    
    //let depth, targetLoc = parse input
    //buildCave depth targetLoc
    //|> assessRisk targetLoc


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)