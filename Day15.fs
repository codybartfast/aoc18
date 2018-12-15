(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day15

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

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
type Unit = Kind * (int * int)
type UnitLocation = Location*Unit
type Square = Wall | Empty | Unit of Unit
type SquareLocation = Location*Square
type Board = Square[,]
type Game = Board * int * bool

(* ================ Part A ================ *) 
let startAP = 3
let startHP = 200

let parseLine line =
    line
    |> toChars
    |> Array.map (function
        |'#' -> Wall
        |'.' -> Empty
        |'E' -> Unit (Elf, (3, 200))
        |'G' -> Unit (Goblin, (3, 200))
        | _ -> failwith "oops")    

let parseInput input = 
    input
    |> toLines
    |> Array.map parseLine
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
      | Wall -> '#'
      | Empty -> '.'
      | Unit (kind, _) -> match kind with Elf -> 'E' | _ -> 'G'

let display (board : Board) = 
    board
    |> Array2D.map displaySquare
    |> toRows
    |> Array.map toString
    |> (String.concat "\n")

let findUnitLocations (board:Square[,])  =
    board
    |> toSequence
    |> Seq.choose (fun (coord, square) -> 
        match square with
        | Unit unit -> Some (coord, unit)
        | _ -> None)
    |> List.ofSeq


let getAdjacent (board:Board) (x,y) =
    [
        (x,y-1), board.[x,y-1]; 
        (x+1,y), board.[x+1,y];
        (x-1,y), board.[x-1,y]; 
        (x,y+1), board.[x,y+1]; 
    ]

let adjecentEnemy (board:Board) ((loc, unit):UnitLocation) : UnitLocation option =
    let (kind, _) = unit
    let targets = 
        getAdjacent board loc
        |> List.choose(fun (otherLoc, square) ->
            match square with 
            | Unit otherUnit -> 
                let (otherKind, _) = otherUnit
                if otherKind <> kind then Some (otherLoc, otherUnit) else None
            | _ -> None)
    match targets with
    | [] -> None
    | _ -> targets |> Seq.minBy (fun (_, (_, (_,hp))) -> hp) |> Some

let adjecentEmpty (board:Board) loc =
    getAdjacent board loc
    |> Seq.choose(fun (loc, square) ->
        match square with
        | Empty -> Some loc
        | _ -> None)
 
 
let extendPath (board:Board) (been:Set<Location>) (path:Location list) =
    let nexts = 
        adjecentEmpty board path.Head
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

let findNextLocation (board:Board) ((loc, unit):UnitLocation) : (int*int) option =
    let allPaths = allPaths board loc
    let pathToEnemy =
        allPaths
        |> Seq.tryPick (fun path -> 
            let loc = path.Head
            let test = (loc, unit)
            match adjecentEnemy board test with
            | Some _ -> Some path
            | _ -> None)
    match pathToEnemy with
    | None -> None
    | Some path -> path |> List.rev |> List.tail |> List.head |> Some    
    
let fight (board:Board) ((aLoc,(aKind,(aAP, aHP))):UnitLocation) ((dLoc,(dKind,(dAP, dHP))):UnitLocation) : Board =
    let dHP' = dHP - aAP
    let defender =
        if dHP' <= 0 then Empty else Unit (dKind, (dAP, dHP'))
    let (x,y) = dLoc
    Array2D.set board x y defender
    board 
    
let moveUnit (board:Board) (((x,y), unit):UnitLocation) ((x', y'):int*int)  =
    Array2D.set board x y Empty
    Array2D.set board x' y' (Unit unit)
    (board, ((x',y'), unit))

let engageUnit (board:Board, seenAction:bool) ((x,y) : Location) : (Board*bool)  =
    match board.[x,y] with
    | Empty -> board, seenAction
    | Unit u ->
        let unitLoc = ((x,y), u)

        match adjecentEnemy board unitLoc  with
        | Some target -> (fight board unitLoc target), true
        | None ->
            match findNextLocation board unitLoc with
            | Some loc -> 
                let (board, unitLoc) = (moveUnit board unitLoc loc)
                match adjecentEnemy board unitLoc with
                | Some target -> (fight board unitLoc target), true
                | None -> board, true
            | None -> board, seenAction

    
let playRound (board:Board, round, _seenAction) =
    let unitLocations = findUnitLocations board |> List.map fst
    let board, sawAction = 
        ((board, false), unitLocations)
        ||> List.fold engageUnit
    let result = board, round+1, sawAction
    Some (result, result)

let battle board =
    (board, 0, false)
    |> Seq.unfold playRound
    |> Seq.map(fun (board, r, s) ->
        //(printfn "%O\n%A\n%O" (display board) board r)
        //Console.ReadKey () |> ignore
        (board, r, s))

let Part1 (input : string) =  // "result1" (*
    let board = parseInput input
    //let game = board, 0, false
    //let (board, round, sawAction) = playRound (board, 0, false)
    //display board
    let board, rounds, _ = 
        battle board
        |> Seq.find (fun (_, round, action) -> 
            let finished = not action
            finished)

    let rounds = rounds - 1

    let survivors = findUnitLocations board |> Seq.map snd
    let hps = 
        survivors |> Seq.sumBy (function (_,(_, hp)) -> hp | _ -> 0)

    (printfn "%O\n%A\n%O x %O" (display board) board rounds hps)
    //Console.ReadKey () |> ignore

    (hps * (rounds), hps * (rounds-1))

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)