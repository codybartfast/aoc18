(* a.cbf.pub/tx/___________________________________________/data.html *)
(*
    This doesn't work!  
    
    I had some vaguely working code that came up with results that 
    sort of bracketed the right answer so (I admit with shame) I got the
    right answer by just trying the values between the two near misses. 

    The close-but-no-cigar code is in Day22a.fs

    The approach below fails with stack overflow but worked with the 
    the small sample data set.
*)

module Day22

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    //|> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) 
    |> List.ofArray
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

type Type = Rocky | Narrow | Wet
type Equiped = Torch | Gear | Neither
type Location = int*int
type Time = int


type History = { Torch: Time option; Gear: Time option; Neither: Time option}
let NoHistory = {Torch = None; Gear = None; Neither = None}

type Region = {Location : Location; Type : Type; Erosion : int; History: History}

type Status = {
    Location: Location;  
    Time : Time; 
    Equiped: Equiped;
    Path: Status list;} 

type CaveMap = Map<Location, Region>
type Cave = {MaxX: int; MaxY : int; Map : CaveMap;}


let parse text  = 
    let lines = text |> toLines
    let depth = rxMatch "\d+" lines.[0] |> (fun m -> m.Value |> int)
    let (x,y) = rxMatch "(\d+),(\d+)" lines.[1] |> (fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2)
    (depth, (x, y))

//let maxes (cave:Cave) =
//    let coords = cave |> Map.toList |> List.map fst
//    let maxX = coords |> List.map fst |> List.max
//    let maxY = coords |> List.map snd |> List.max
//    (maxX, maxY)

let display tLoc (cave:Cave)  =
    let symbol {Type=typ} = 
        match typ with
        | Rocky -> '.'
        | Wet -> '='
        | Narrow -> '|'
    let maxX, maxY = cave.MaxX, cave.MaxY
    let array = 
        Array.init (maxY+1)  (fun  y -> 
            Array.init (maxX+1) (fun x -> 
                cave.Map.[(x,y)] |> function
                    | {Location = (0,0)} -> 'M'
                    | {Location = loc} when loc = tLoc-> 'T'
                    | r -> symbol r))
    array
    |> Array.iter (fun (line:char[]) -> printfn "%s" (toString line))
    cave

let getType erosion =
    match erosion % 3 with
    | 0 -> Rocky
    | 1 -> Wet
    | 2 -> Narrow

let newRegion depth tLoc (map:CaveMap) loc =
    let erosion  =
        let (x,y) = loc
        let geoIndex =
            if loc = (0,0) || loc = tLoc then 0 else
            if y = 0 then x * 16807 else
            if x = 0 then y * 48271 else
            map.[x-1,y].Erosion * map.[x,y-1].Erosion
        (geoIndex + depth) % 20183
    {Location = loc; Type = (getType erosion); Erosion = erosion; History = NoHistory}

let addRegion depth tLoc (map:CaveMap) loc =
    let region = newRegion depth tLoc map loc
    map.Add (region.Location, region)

let buildCave depth tLoc  : Cave =
    let (X,Y) = tLoc
    let map = 
        ((Map.empty),
            [for y in 0..Y do for x in 0..X do yield (x,y)])
        ||> List.fold (addRegion depth tLoc)
    {Map=map; MaxX = X; MaxY = Y}

let rec extendCave depth (newX,newY) (cave:Cave)  =
    
    if newX <= cave.MaxX && newY <= cave.MaxY  then cave else
    // across
    let X,Y = cave.MaxX, cave.MaxY
    let map = 
        (cave.Map,
            seq{ for y in 0..Y do for x in X+1..newX do yield (x,y)})
        ||> Seq.fold (addRegion depth (-1,-1))
    // down
    let map = 
        (map,
            seq{for y in Y+1..newY do for x in 0..X do yield (x,y)})
        ||> Seq.fold (addRegion depth (-1,-1))
    // bottom right
    let map = 
        (map,
            seq{for y in Y+1..newY do for x in X+1..newX do yield (x,y)})
        ||> Seq.fold (addRegion depth (-1,-1))
    {Map=map; MaxX = max X newX; MaxY = max Y newY}

let assessRisk (X,Y) (cave:Cave) =
    cave.Map
    |> Map.toSeq
    |> Seq.filter (fun ((x,y), _) -> x <= X && y <= Y)
    |> Seq.map snd
    |> Seq.sumBy (fun {Type=typ} -> 
        match typ with Rocky -> 0 | Wet -> 1 | Narrow -> 2)

let Part1 (input : string) = //  "result1" 
    let depth, targetLoc = 510, (10,10)
    buildCave depth targetLoc
    |> extendCave depth (15,15)
    |> display targetLoc
    |> assessRisk targetLoc
    
    //let depth, targetLoc = parse input
    //buildCave depth targetLoc
    //|> assessRisk targetLoc

(* ================ Part B ================ *)


let equip2 equiped typFrom typTo =
    if typFrom = typTo then [(equiped, 1)] else
    let (opt1, opt2) =
        match typTo with
        | Rocky -> (Torch, Gear)
        | Narrow -> (Neither, Torch)
        | Wet -> (Gear, Neither)
    
    let delay (opt : Equiped) : int = if opt = equiped then 1 else 8
    [(opt1, (delay opt1)); (opt2, (delay opt2))]

let startingPathfinder =
    let mouth = (0,0)
    {   Location = mouth
        Time = 0
        Path = []
        Equiped = Torch }

let getNext tLoc depth (status:Status) (cave:Cave) (nx,ny) =
    //if (nx,ny) = (11,785) then printfn "hey"
    let (x,y) = status.Location
    let here = cave.Map.[x,y]
    let cave = extendCave depth (nx,ny) cave
    let next = cave.Map.[nx,ny]
    let options =
        (equip2 status.Equiped here.Type next.Type)
        |> List.map (fun (equipment, delay) -> 
            let delay =
                if (nx,ny) = tLoc && status.Equiped <> Torch 
                then 8 else delay
            let time = status.Time + delay
            {   Location = nx,ny
                Time = time
                Path = status::(status.Path)
                Equiped = equipment })
    (cave, options)

let getHistory (cave:Cave) loc equip =
    let history = cave.Map.[loc].History 
    match equip with
    | Torch -> history.Torch
    | Gear -> history.Gear
    | Neither -> history.Neither

let updateCave (status:Status) (cave:Cave) =
    let newHistory history =
        let equip, time = status.Equiped, status.Time
        let current =
            match equip with
            | Torch -> history.Torch
            | Gear -> history.Gear
            | Neither -> history.Neither
        match current with
        Some curTime when time >= curTime -> failwith "really?" 
        |_ ->
        match equip with
        | Torch -> {history with Torch= Some time}
        | Gear -> {history with Gear= Some time}
        | Neither -> {history with Neither= Some time}
    let region = cave.Map.[status.Location]
    let newRegion = {region with History = newHistory region.History}
    {cave with Map = cave.Map.Add (status.Location, newRegion)}
   

let findAPath depth (tx, ty) (cave:Cave) =
    let rec find (pf:Status) cave = 
        match pf.Location with
        | x,y when x < tx -> 
            let (cave, options) = getNext (tx, ty) depth pf cave (x+1, y)
            let option = options.Head
            find option cave
        | x,y when y < ty ->             
            let (cave, options) = getNext (tx, ty) depth pf cave (x, y+1)
            let option = options.Head
            find option cave
        | x,y when x=tx && y=ty -> pf
        | _ -> failwith "oops"
    find startingPathfinder cave

let adjacent (pf:Status) =
    let x,y = pf.Location
    seq{
        if y > 0 then yield (x, y-1)
        if x > 0 then yield (x-1, y)
        yield (x+1,y)
        yield (x,y+1) }

let manhatton (x,y) (X, Y) = (abs x - X) + (y - Y)

let debug1 status cave =
        let th::tp =  [ (4,1); (3,1); (2,1); (1,1); (1,0); (0,0);]
        //let th::tp =  [(2, 1); (1,1); (0,1); (0,0);]
        let path = status.Path |> List.map (fun s -> s.Location)
        let loc = status.Location
        if loc = th && path = tp then
            printfn "--- Cave: %i  --- Loc: %O (%imin) using %O" cave.Map.Count status.Location status.Time status.Equiped
            let adjs = adjacent status
            adjs |> Seq.iter(fun adj -> 
                let text = sprintf "       %O - %O" adj cave.Map.[adj].History
                printfn "%s" (text.Replace("\n", "")))
            Console.ReadKey() |> ignore

let debug2 tLoc status cave =
    let loc = tLoc
    let time = 45
    //let equip = Gear
    if status.Location = loc
        && status.Time = time
        //&& status.Equiped = equip 
        then
            printfn "--- Cave: %i  --- Loc: %O (%imin) using %O" cave.Map.Count status.Location status.Time status.Equiped
            status.Path
            |> List.iter (fun s -> 
                printfn "       Loc:%O  Time:%O  Equp:%O" s.Location s.Time s.Equiped
            )

type BetterResult = No of Cave | Yes of (Cave * Status)

let findBetterPath depth tLoc (current:Status) (cave:Cave) : BetterResult =
    let max = current.Time
    let distance status = manhatton status.Location tLoc
    let rec find status cave : BetterResult = 
        if status.Location = tLoc then
            if status.Time < current.Time then
                Yes (cave, status)
            else
                find status.Path.Head cave
        else
            let checkAndFind next cave =
                if next.Time >= max then No cave else
                match getHistory cave next.Location next.Equiped with
                | Some prevTime when next.Time >= prevTime -> No cave
                | _ ->
                find next ( updateCave next cave)                    
            let rec explore cave adjacents =
                match adjacents with
                | [] -> No cave
                | adj::rest -> 
                    let cave, nexts = getNext tLoc depth status cave adj
                    let result = 
                        match nexts with
                        | [next] -> checkAndFind next cave
                        | next1::[next2] ->
                            match checkAndFind next1 cave with
                            | Yes path -> Yes path
                            | No cave -> checkAndFind next2 cave
                    match result with
                    | Yes path -> Yes path
                    | No cave -> explore cave rest
            let adjacents = adjacent status |> List.ofSeq
            match explore cave adjacents with
            | Yes path -> Yes path
            | No cave ->
                match status.Path with
                | head::_ ->  find head cave
                | _ -> No cave
    find current cave

let Part2 result1 (input : string) = // "result2" (*
    let depth, targetLoc = 510, (10,10)
    let depth, targetLoc = parse input
    let oCave = buildCave depth targetLoc
    let aPath = findAPath depth targetLoc oCave

    let cave = 
        (oCave, aPath.Path)
        ||> List.fold (fun cave status -> updateCave status cave)

    (cave, aPath)
    |> Seq.unfold (fun (cave, path) ->
        print path.Time
        let result = findBetterPath depth targetLoc path cave
        match result with
        |Yes (newCave, newPath) -> Some (newPath, (newCave, newPath))
        |No cave -> None)
    |> Seq.last
    |> (fun last -> last.Time)
     
    

//*)