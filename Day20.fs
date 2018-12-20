(* a.cbf.pub/tx/beTdFw18eGqEYSuyYMCWLWgLNXrPlLAJj2H5sexzfVc/data.html *)

module Day20

#nowarn "0025"

open System

let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

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

let display (map:Mp) =
    let fromSquare = function
        | None -> '#'
        | Some {Kind=Origin} -> 'X'
        | Some {Kind=Wall} -> '█'
        | Some {Kind=Room} -> ' '
        | Some {Kind=Unknown} -> '?'
        | Some {Kind=(Door Horiz)} -> '-'
        | Some {Kind=(Door Vert)} -> '|' 
                
    let coords = map |> Map.toList |> List.map fst
    let minX = coords |> List.map fst |> List.min
    let maxX = coords |> List.map fst |> List.max
    let minY = coords |> List.map snd |> List.min
    let maxY = coords |> List.map snd |> List.max

    let zerodMap =
        map
        |> Map.toSeq
        |> Seq.map (fun ((x,y), square) -> ((x-minX,y-minY), square))
        |> Map
    let (X, Y) = (1 + maxX-minX, 1 + maxY-minY)

    let array = 
        Array.init (Y)  (fun  y -> 
            Array.init (X) (fun x -> 
                zerodMap.TryFind (x,y) |> fromSquare))
    array
    |> Array.iter (fun (line:char[]) -> printfn "%s" (toString line))
    map

let adjacent (x,y) =
    [ (x-1, y-1); (x, y-1); (x+1, y-1);
      (x-1, y);               (x+1, y);
      (x-1, y+1); (x, y+1); (x+1, y+1)]   
   
let addToMap square map =    
    let loc = (square.X, square.Y)
    // If there's already a square here use it     
    match Map.tryFind loc map with
    | Some square when square.Kind <> Unknown -> (map, square)
    | _ -> 
    let map = map.Add (loc, square)
    // Set any empty surrounding squares as Unknown
    let map = 
        (map, adjacent loc)
        ||> List.fold (fun map (x,y) -> 
            if map.ContainsKey (x,y) then map else
            map.Add ((x,y), {Kind=Unknown; X=x; Y=y; Doors=square.Doors}))
    (map, square)

let addRoom prev dir map =
    let {X=x; Y=y; Doors=doors } = prev
    let (door, room) = 
        match dir with
        | 'N' -> 
            {Kind = (Door Horiz); X=x; Y=y-1; Doors=doors},
                {Kind = Room; X=x; Y=y-2; Doors=doors+1}
         | 'W' -> 
            {Kind = (Door Vert); X=x-1; Y=y; Doors=doors},
                {Kind = Room; X=x-2; Y=y; Doors=doors+1}
        | 'E' ->
            {Kind = (Door Vert); X=x+1; Y=y; Doors=doors},
                {Kind = Room; X=x+2; Y=y; Doors=doors+1}
        | 'S' ->
            {Kind = (Door Horiz); X=x; Y=y+1; Doors=doors},
                {Kind = Room; X=x; Y=y+2; Doors=doors+1}
    let (map, room) = 
        addToMap door map |> fst |> addToMap room 
    (room, map)

let rec build (state : State) =
    let regex, prev, altStarts, map = state
    let head::tail = regex
    let altStart::altStartsTail = altStarts
    match head with
    | '$' -> state
    | '(' -> 
        build (tail, prev, prev::altStarts, map)
    | '|' -> 
        build (tail, altStart, altStarts, map)
    | ')' ->
        build (tail, prev, altStartsTail, map)
    | 'N' | 'W' | 'E' | 'S' as dir -> 
        let (room, map) = addRoom prev dir map
        build (tail, room, altStarts, map)

let finish map =
    map
    |> Map.map (fun _ square ->
        match square with
        | {Kind=Unknown} -> {square with Kind=Wall}
        | _ -> square)    

let parse input =
    let regex = input |> toChars |> List.ofArray
    let origin = {Kind=Origin; X=0; Y=0; Doors=0}
    let (map, _) = addToMap origin Map.empty
    let  (_, _, _, map) = 
        build (regex.Tail, origin, [origin], (map.Add ((0,0), origin)))
    finish map

let maxDoors map =
    map
    |> Map.toSeq
    |> Seq.filter (fun (_, {Kind=kind}) -> kind=Room)
    |> Seq.map (fun (_, {Doors=doors}) -> doors)
    |> Seq.max 

let Part1 (input : string) = 
    parse input |> display |> maxDoors

(* ================ Part B ================ *)

let thousands map =
    map
    |> Map.toSeq
    |> Seq.filter (fun (_, {Kind=kind; Doors=doors}) -> 
        kind=Room && doors >= 1000)
    |> Seq.length

let Part2 result1 (input : string) =
    parse input |> thousands
