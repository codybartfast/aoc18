(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day20

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Data.SqlTypes

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
type Orient = Vert | Horiz
type Kind = Origin | Room | Door of Orient | Wall | Unknown
type Square = {Kind : Kind; X : int; Y : int; Doors : int}
type Loc = (int*int)
type Mp = Map<Loc,Square>
type Prev = Square   
type Rx = char list
type AltStarts = Square list
type State = Rx * Prev * AltStarts * Mp

let parseLine  = 
    rxMatch "(-?\d+)\D+?(-?\d+)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grpi 1, grpi 2

let display (map:Mp) =
    let b = map.Count

    let coords = map |> Map.toList |> List.map fst
    let minX = coords |> List.map fst |> List.min
    let maxX = coords |> List.map fst |> List.max
    let minY = coords |> List.map snd |> List.min
    let maxY = coords |> List.map snd |> List.max

    let map =
        map
        |> Map.toSeq
        |> Seq.map (fun ((x,y), square) -> ((x-minX,y-minY), square))
        |> Map
    let (X, Y) = (1 + maxX-minX, 1 + maxY-minY)
    let b = map.Count

    let array = 
        Array.init (Y)  (fun  y -> 
            Array.init (X) (fun x -> 
                match map.TryFind (x,y) with
                | None -> ' '
                | Some {Kind=Origin} -> 'X'
                | Some {Kind=Wall} -> '#'
                | Some {Kind=Room} -> '.'
                | Some {Kind=Unknown} -> '?'
                | Some {Kind=(Door Horiz)} -> '-'
                | Some {Kind=(Door Vert)} -> '|' ))
    array
    |> Array.iter (fun (line:char[]) -> printfn "%s" (toString line))
   
let addMap orient square map : (Mp * Square) =    
    let loc = (square.X, square.Y)
    let (x,y) = loc    
    let add x y kind (map:Mp) =
        Map.add (x,y) {Kind=kind; X=x; Y=y; Doors=square.Doors} map

    let square =
        if square.Kind <> Room then square else
        let existing = Map.tryFind loc map
        match existing with
        | None | Some {Kind=Unknown} -> square
        | Some {Kind=Room} as room -> existing.Value
        | _ -> failwith "oops!"

    let map = 
        (map, 
            [(x-1, y-1); (x, y-1); (x+1, y-1);
            (x-1, y);           (x+1, y);
            (x-1, y+1); (x, y+1); (x+1, y+1)])
        ||> List.fold (fun map (x,y) -> 
            match Map.containsKey (x,y) map with
            | true -> map
            | false -> add x y Unknown map)

    let map = Map.add loc square map
    let map =
        match (square.Kind, orient) with
        | Door _, Horiz -> 
            map |> add x (y-1) Wall |> add x (y+1) Wall
        | Door _, Vert -> 
            map |> add (x-1) y Wall |> add (x+1) y Wall
        | _ -> map
    (map, square)

let rec build (state : State)  : State  =
    let regex, prev, altStarts, map = state
    let head::tail = regex
    let altStart::otherStarts = altStarts
    match head with
    | '$' -> state
    | '(' -> 
        build (tail, prev, prev::altStarts, map)
    | '|' -> 
        build (tail, altStart, altStarts, map)
    | ')' ->
        build (tail, prev, otherStarts, map)
    | 'N' | 'W' | 'E' | 'S' -> 
        let (_, room, _, map) = addRoom state
        build (tail, room, altStarts, map)

and addRoom (state: State) : State =
    let regex, prev, altStarts, map = state
    let head::tail = regex
    let {Kind=kind; X=x; Y=y; Doors=doors } = prev
    match head with
    | 'N' -> 
        let door = {Kind = (Door Horiz); X=x; Y=y-1; Doors=doors}
        let room = {Kind = Room; X=x; Y=y-2; Doors=doors+1}
        let (map, room) = addMap Vert door map |> fst |> addMap Vert room 
        (tail, room, altStarts, map)
    | 'W' -> 
        let door = {Kind = (Door Vert); X=x-1; Y=y; Doors=doors}
        let room = {Kind = Room; X=x-2; Y=y; Doors=doors+1}
        let (map, room) = addMap Horiz door map |> fst |> addMap Horiz room
        (tail, room, altStarts, map)
    | 'E' -> 
        let door = {Kind = (Door Vert); X=x+1; Y=y; Doors=doors}
        let room = {Kind = Room; X=x+2; Y=y; Doors=doors+1}
        let (map, room) = addMap Horiz door map |> fst |> addMap Horiz room
        (tail, room, altStarts, map)
    | 'S' -> 
        let door = {Kind = (Door Horiz); X=x; Y=y+1; Doors=doors}
        let room = {Kind = Room; X=x; Y=y+2; Doors=doors+1}
        let (map, room) = addMap Vert door map |> fst |> addMap Vert room 
        (tail, room, altStarts, map)

let finish map =
    map
    |> Map.map (fun key square ->
        match square with
        | {Kind=Unknown} -> {square with Kind=Wall}
        | _ -> square)    
 
let maxDoors map =
    map
    |> Map.toSeq
    |> Seq.filter (fun (_, {Kind=kind}) -> kind=Room)
    |> Seq.map (fun (_, {Doors=doors}) -> doors)
    |> Seq.max 

let thousands map =
    map
    |> Map.toSeq
    |> Seq.filter (fun (_, {Kind=kind; Doors=doors}) -> kind=Room && doors >= 1000)
    |> Seq.length

let Part1 (input : string) =  // "result1" (*
    let regex = input |> toChars |> List.ofArray
    let map = Map.empty
    let origin = {Kind=Origin; X=0; Y=0; Doors=0}
    let (map, _) = addMap Vert origin map
    let  (_, _, _, map) = build (regex.Tail, origin, [origin], (map.Add ((0,0), origin)))
    let map = finish map
    display map
    thousands map
//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)