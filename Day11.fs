(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day11

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Net.NetworkInformation

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

let power serial (x,y) =
     let rack = x + 10
     let big = ((rack * y) + serial) * rack
     ((big % 1000) / 100) - 5

let squares (gx, gy) (sx, sy) =
    seq{ for x in [1..(gx - (sx-1))] do  
            for y in [1..(gy - (sy-1))] do
                yield 
                    [y..(y+(sy-1))] |> List.map (fun y -> [x..(x+(sx-1))] |> List.map (fun x -> (x,y))) }

let squarePower serial (square : (int * int) list list) =
    square
    |> List.collect id
    |> List.sumBy (power serial)
    

let Part1 (input : string) = "r1" (*
    let serial = input |> int
    let squares = squares (300,300) (3,3)
    
    let serial = 9810

    squares
    |> Seq.map (fun square -> (square.Head.Head, squarePower serial square))
    |> Seq.maxBy snd
    |> fst
//*)
    

(* ================ Part B ================ *)

let simplePower (powerGrid : int[][]) square =
    square
    |> List.collect id
    |> List.sumBy (fun (x,y) -> powerGrid.[y-1].[x-1])

//let getMaxForSize powerGrid size =
//    let squares = squares (300,300) (size,(print size))
//    squares
//    |> Seq.map (fun square -> (square.Head.Head, simplePower powerGrid square))
//    |> Seq.maxBy snd
//    |> (fun ((x,y), p) -> ((x,y,size), p))

let topSquares gSize size =
    [1..(gSize-(size-1))]
    |> Seq.map (fun X -> 
        [1..size] |> List.map (fun y -> [X..(X+(size-1))] |> List.map (fun x -> (x,y))))

let colValues (powerGrid : int[][]) (topSquare : (int * int) list list) =
    let gSize = powerGrid.Length
    let sSize = topSquare.Length
    let (firstX, firstY) = topSquare.Head.Head
    let lastX = firstX + (sSize - 1)
    let topValue = simplePower powerGrid topSquare
    seq{    
        yield! ((((firstX, firstY, sSize), topValue), [(sSize+1)..gSize])
                ||> List.scan (fun (_, prevPower) newY -> 
                    let oldY = newY - sSize
                    let newPower = Array.sum powerGrid.[newY-1].[(firstX-1)..(lastX-1)]
                    let oldPower = Array.sum powerGrid.[oldY-1].[(firstX-1)..(lastX-1)]
                    let power = prevPower + (newPower - oldPower)
                    ((firstX, (oldY + 1), sSize), power)))}
    
let getMaxForSize powerGrid gSize sSize =
    let topSquares = topSquares gSize (print sSize)
    topSquares
    |> Seq.collect (colValues powerGrid)
    |> Seq.maxBy snd

let Part2 result1 (input : string) = // "result2" 
    let serial = input |> int
    
    let serial = 9810

    let powerGrid = 
        [|1..300|]
        |> Array.map (fun y -> [|1..300|] |> Array.map (fun x -> power serial (x,y)))

    //getMaxForSize powerGrid 300 3

    [1..300]
    |> Seq.map (fun size -> getMaxForSize powerGrid 300 size)
    |> Seq.maxBy snd
    |> fst


//