(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day15Fiddle

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Drawing
open System.IO
open System.Linq

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    //|> List.ofArray
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

type Kind = Elf | Goblin
type Location = int*int
type Unit = string * Kind * (int * int)
type UnitLocation = Location*Unit
type Square = Wall | Empty | Unit of Unit
type SquareLocation = Location*Square
type Board = Square[,]
type Game = Board * int * bool

(* ================ Part A ================ *) 
let startHP = 200

let parseLine hp lineNo line =
    line
    |> toChars
    |> Array.mapi (fun chrNo chr ->
    let id = sprintf "%O:%O" lineNo chrNo
    match chr with
        |'#' -> Wall
        |'.' -> Empty
        |'E' -> Unit (id, Elf, (hp, 200))
        |'G' -> Unit (id, Goblin, (3, 200))
        | _ -> failwith "oops")    

let parseInput hp input =
    input
    |> toLines
    |> Array.mapi (parseLine hp)
    |> fun nested ->
        let X = nested.[0].Length
        let Y = nested.Length        
        Array2D.init X Y (fun x y -> nested.[y].[x])

let toRows array2D =
    let X = Array2D.length1 array2D
    let Y = Array2D.length2 array2D
    [|for y in seq{0..Y-1} -> y|]
    |> Array.map (fun y ->
        [|for x in seq{0..X-1} -> x|]
        |> Array.map (fun x -> array2D.[x,y]))

let toSequence array2D =
    let X = Array2D.length1 array2D
    let Y = Array2D.length2 array2D
    seq{for y in seq{0..Y-1} do yield! seq{for x in seq{0..X-1} do yield x, y}}
    |> Seq.map (fun (x, y) -> ((x,y), array2D.[x,y]))

let displaySquare = function
      | Wall -> "████"
      | Empty -> "    "
      | Unit (_, kind, (_, hp)) -> 
        match kind with 
        |Elf -> "E"+hp.ToString().PadLeft(3)
        |Goblin -> "G"+hp.ToString().PadLeft(3)


let display (board : Board) = 
    board
    |> Array2D.map displaySquare
    |> toRows
    |> Array.map (String.concat "")
    |> (String.concat "\n")

let findUnitLocations (board:Square[,])  =
    board
    |> toSequence
    |> Seq.choose (fun (coord, square) -> 
        match square with
        | Unit unit -> Some (coord, unit)
        | _ -> None)
    |> List.ofSeq

let getEnemies kind board = 
    board
    |> findUnitLocations
    |> List.filter (snd >> (fun (_, other ,_) -> other <> kind))

let countEnemies kind board =
    getEnemies kind board
    |> Seq.length

let hasEnemies board ((_, (_,kind,_)):UnitLocation) =
    countEnemies kind board > 0

let countElves = countEnemies Goblin

let distance (x,y) (x',y') = abs (x - x') + abs (y - y')

let areAdjacent (x,y) (x',y') = 
    distance (x,y) (x',y') = 1

let getAdjacent (board:Board) (x,y) = 
     [  
        ((x,y-1), board.[x,y-1]); 
        ((x-1,y), board.[x-1,y]); 
        ((x+1,y), board.[x+1,y]);
        ((x,y+1), board.[x,y+1]);]

let adjacentEnemies (board:Board) loc kind =
    getAdjacent board loc
        |> List.choose(fun (otherLoc, square) ->
            match square with 
            | Unit otherUnit -> 
                let (_, otherKind, _) = otherUnit
                if otherKind <> kind then Some (otherLoc, otherUnit) else None
            | _ -> None)    

let adjecentEnemy (board:Board) ((loc, unit):UnitLocation) : UnitLocation option =
    let (_, kind, _) = unit
    let targets = adjacentEnemies board loc kind       
    match targets with
    | [] -> None
    | _ -> targets |> Seq.sortBy (fun ((x,y), (_, _,(_,hp))) -> (hp,y,x)) |> Seq.head |> Some

let adjacentEmpty (board:Board) loc =
    getAdjacent board loc
    |> Seq.choose(fun (loc, square) ->
        match square with
        | Empty -> Some loc
        | _ -> None)
 
 
let extendPath (board:Board) (been:Set<Location>) (path:Location list) =
    let nexts = 
        adjacentEmpty board path.Head
        |> Seq.filter (fun loc -> not (been.Contains loc))
        |> List.ofSeq
    let newBeen = Set.union been (Set nexts)
    let newPaths =
        nexts |> Seq.map (fun newLoc -> newLoc::path)
    (newPaths, newBeen)

let extendPaths board (been:Set<Location>) (paths:seq<Location list>) =
    let (newPathsSeq, been) = 
        (been, paths)
        ||> Seq.mapFold (extendPath board)
    (newPathsSeq |> Seq.collect id, been)


let rec allPaths board (loc : int*int) =
    let b = Set [loc]
    let p = seq{yield [loc]}
    (b, p)
    |> Seq.unfold (fun (been, (prevPaths : seq<Location list>)) ->
        let (newPaths, newBeen) = extendPaths board been prevPaths
        match Seq.isEmpty newPaths with
        | true -> None
        | false -> Some (newPaths, (newBeen, newPaths)))
    |> Seq.collect id
    //|> List.ofSeq ///////////

let enemyRanges board kind =
    let enemyRange (x,y) =
        [(1, (x, y-1)); (2, (x-1,y)); (3, (x+1,y)); (4, (x,y+1)) ]    
    getEnemies board kind
    |> List.collect (fst >>(enemyRange))
   
let findNextLocation (board:Board) ((loc, unit):UnitLocation) : (int*int) option =
    //printfn "%s" (display board)
    let allPathsFromUnit = allPaths board loc |> List.ofSeq
    let pathsToEnemy =
        allPathsFromUnit
        |> Seq.choose (fun path -> 
            let loc = path.Head
            let test = (loc, unit)
            match adjecentEnemy board test with
            | Some _ -> Some path
            | _ -> None)
        |> List.ofSeq
    if pathsToEnemy.IsEmpty then None else
    let nearest = 
        pathsToEnemy
        |> List.groupBy (List.length)
        |> List.sortBy fst
        |> List.head
        |> snd
    let chosenLoc = 
        nearest
        |> List.groupBy (fun path -> 
            let (x,y) = path.Head
            (y,x))
        |> List.sortBy fst
        |> List.head
        |> snd
        |> List.exactlyOne
        |> List.head

    if areAdjacent loc chosenLoc then Some chosenLoc else
    let allPathsFromChosen = allPaths board chosenLoc |> List.ofSeq
    let potentialStarts = adjacentEmpty board loc |> Set
    
    let shortestFrom =
        allPathsFromChosen
        |> List.filter (fun path -> 
            potentialStarts.Contains path.Head)
        |> List.groupBy (List.length)
        |> List.sortBy fst
        |> List.head
        |> snd
    let startChoices =     
        shortestFrom
        |> List.map List.head
    if startChoices.Length > 1 then printf "" else printf ""
    let start =
        startChoices
        |> Seq.sortBy (fun (x,y) -> (y,x))
        |> Seq.head
    Some start

let fight (board:Board) ((aLoc,(_, aKind,(aAP, aHP))):UnitLocation) ((dLoc,(dId, dKind,(dAP, dHP))):UnitLocation) : Board =
    let dHP' = dHP - aAP
    let defender =
        if dHP' <= 0 then Empty else Unit (dId, dKind, (dAP, dHP'))
    let (x,y) = dLoc
    Array2D.set board x y defender
    board 
    
let moveUnit (board:Board) (((x,y), unit):UnitLocation) ((x', y'):int*int)  =
    Array2D.set board x y Empty
    Array2D.set board x' y' (Unit unit)
    (board, ((x',y'), unit))

let engageUnit (board:Board, prevSawAction) (((x,y), _) : UnitLocation) : (Board*bool)  =
     match board.[x,y] with
    | Unit unit ->
        let unitLoc = ((x,y),unit)
        match adjecentEnemy board unitLoc  with
        | Some target -> (fight board unitLoc target), true
        | None ->
            match findNextLocation board unitLoc with
            | Some loc -> 
                let (board, unitLoc) = (moveUnit board unitLoc loc)
                match adjecentEnemy board unitLoc with
                | Some target -> (fight board unitLoc target), true
                | None -> board, true
            | None -> board, (hasEnemies board unitLoc)
    | _ -> board, prevSawAction

    
let playRound (board:Board, round, _seenAction) =
    let unitLocations = findUnitLocations board 
    //let info = unitLocations |> List.map (fun (loc, (_, kind, (_,hp))) -> (kind, hp))
    //printfn "\n%O" (display board) 
    ////Console.ReadKey () |> ignore    /////////////////////////////////////////////////
    let board, sawAction = 
        ((board, false), unitLocations)
        ||> List.fold engageUnit
    let result = board, round+1, sawAction
    Some (result, result)

let battle board =
    (board, 0, false)
    |> Seq.unfold playRound

let Part1 (input : string) =  "result1" (*
    let board = parseInput 3 input ////////////////////////////

    printfn "%O" (display board)
    printfn ""
            
    let board, rounds, _ = 
        battle board
        |> Seq.takeWhile (fun (_, _, action) -> action)    
        |> Seq.last 

    let survivors = findUnitLocations board |> Seq.map snd
    let hps = 
        survivors |> Seq.sumBy (function (_,_,(_, hp)) -> hp | _ -> 0)

    (printfn "%O\n%O x %O" (display board) rounds hps)
 
    hps * (rounds)
//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) = // "result2" (*
    //if true then 0 else
    let (hp, board, rounds) =
        seq{15..Int32.MaxValue}
        |> Seq.pick(fun hp ->
            let board = parseInput hp input
            let elfCount = countElves board
    
            let board, rounds, _ = 
                battle board
                |> Seq.takeWhile (fun (_, _, action) -> action)    
                |> Seq.last    
        
            let surviving = countElves board
        
            if surviving = elfCount 
                then Some (hp, board, rounds)
                else None)

    let survivors = findUnitLocations board |> Seq.map snd
    let hps = 
        survivors |> Seq.sumBy (function (_,_,(_, hp)) -> hp | _ -> 0)

    printfn "%O" (display board)
    (printfn "(%O x %O)" rounds hps)

    hps * (rounds)

//*)