(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day13

//#nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
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
let toHex = BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

type Turn = Straight | Left | Right
type Cart = char * Turn * bool
type Railway = (char * Cart)[,]
[<Literal>] 
let NoCart = '_'

let parseInput input : Railway = 
    let lines = input |> toLines |> Array.ofList
    let squares =
        lines 
        |> Array.map (fun line ->
            line |> toChars |> Array.map (fun chr ->
            let rail = 
                match chr with
                | '<' | '>' -> '-'
                | 'v' | '^' -> '|'
                | chr -> chr
            let cart = 
                match chr with
                | '<' | '^' | 'v' | '>' as cart -> cart
                | _ -> NoCart
            (rail, (cart, Left, false))))
    let X = squares.[0].Length
    let Y = squares.Length
    let railway = Array2D.init X Y (fun x y -> squares.[y].[x])
    railway

let display (track :Railway) = 
    printfn "============================"
    [Array2D.base2 track .. -1 + Array2D.length2 track ]
    |> Seq.map (fun y -> 
        track.[*,y] 
        |> Seq.map (fun (track, (dir, turn, _)) -> if dir = '_' then track else dir)
        |> toString)
    |> Seq.iter(fun line -> printfn "%O" line)

let printRail railway =
    display railway
    railway

let evalPoint (railway :Railway) (x,y) =
    let rec newCart track dir turn = 
        match track, dir, turn with
        | _, NoCart, _ -> (x, y), dir, turn

        | '-', '>', _ -> (x+1, y), dir, turn
        | '-', '<', _ -> (x-1, y), dir, turn
        | '|', '^', _ -> (x, y-1), dir, turn
        | '|', 'v', _ -> (x, y+1), dir, turn

        | '/', '>', _ -> newCart '|' '^' turn
        | '/', '<', _ -> newCart '|' 'v' turn
        | '/', '^', _ -> newCart '-' '>' turn
        | '/', 'v', _ -> newCart '-' '<' turn

        | '\\', '>', _ -> newCart '|' 'v' turn
        | '\\', '<', _ -> newCart '|' '^' turn
        | '\\', '^', _ -> newCart '-' '<' turn
        | '\\', 'v', _ -> newCart '-' '>' turn

        | '+', '>', Left -> newCart '|' '^' Straight
        | '+', '>', Straight -> newCart '-' '>' Right
        | '+', '>', Right -> newCart '|' 'v' Left

        | '+', '<', Left -> newCart '|' 'v' Straight
        | '+', '<', Straight -> newCart '-' '<' Right
        | '+', '<', Right -> newCart '|' '^' Left

        | '+', '^', Left -> newCart '-' '<' Straight
        | '+', '^', Straight -> newCart '|' '^' Right
        | '+', '^', Right -> newCart '-' '>' Left

        | '+', 'v', Left -> newCart '-' '>' Straight
        | '+', 'v', Straight -> newCart '|' 'v' Right
        | '+', 'v', Right -> newCart '-' '<' Left
        
        | _ -> failwith "Where to go? Where to go!"
    let (track, (dir, turn, moved)) = railway.[x,y]
    if moved then railway else
    let (newX, newY), newDir, newTurn =
        newCart track dir turn
    railway.[x,y] <- (track, (NoCart, Straight, false))
    let target = railway.[newX, newY]
    if target |> snd |> (fun (cart, _, _) -> cart = NoCart)
    then 
        railway.[newX,newY] <- (fst target, (newDir, newTurn, true))
        railway
    else 
        railway.[newX,newY] <- (fst target, (NoCart, Left, false))
        railway
        

let enumCoords railway =
    seq{ for y in Array2D.base2 railway .. -1 + Array2D.length2 railway do 
                yield! seq {for x in Array2D.base1 railway .. -1 + Array2D.length1 railway do yield (x,y)}}

let clearMovedFlag (railway :Railway) =
    enumCoords railway
    |> Seq.iter (fun (x,y) ->
        let (track, (dir, turn, moved)) = railway.[x,y]
        if moved then
            railway.[x,y] <- (track, (dir, turn, false))
        else ())
    railway

let oneLeft (railway :Railway) =
    let carts = 
        enumCoords railway
        |> Seq.filter (fun (x,y) ->
            let (track, (dir, turn, moved)) = railway.[x,y]
            dir <> NoCart)
        |> List.ofSeq
    let count = carts.Length
    if count > 1 then None
    else
    carts
    |> List.exactlyOne
    |> Some

let evalRailway railway =
    let coords = enumCoords railway
    (railway, coords) 
    ||> Seq.fold (fun railway coord ->
        let newRailway = evalPoint railway coord
        newRailway)
    |> clearMovedFlag

let Part1 (input : string) =  // "result1" (*
    let railway = parseInput input
    let rec chooChoo railway =    
        //display railway
        match oneLeft railway with
        | None -> chooChoo (evalRailway railway)
        | Some (x,y) -> (x,y)
    chooChoo railway

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)