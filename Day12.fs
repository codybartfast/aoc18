(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day12

// #nowarn "0025"

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
let maxPot = 200
let plant = '#'
let noPlant = '.'

let parseInitial  = 
    rxMatch " ([\.\#]+)" 
    >> fun mtch ->
        (groupValue mtch 1)
        |> toChars
    
let parseTansforms  = 
    rxMatch "(\S+) => (\S)" 
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        (grp 1), char (grp 2)

let newPlants () = Array.init (1 + (2*maxPot)) (fun _ -> '.')

let getPlant (plants:char[]) i =
    plants.[i+maxPot]

let getNear (plants:char[]) (i:int) :string =
    let start = i+maxPot-2
    let near = plants.[start..(start+4)] 
    near |> String

let setPlant (plants:char[]) i alive =
    plants.[i+maxPot] <- alive

let plantsValue plants =
    plants
    |> Seq.mapi (fun i havePlant -> if havePlant = plant then i - maxPot else 0)
    |> Seq.sum

let transform transforms plants i =
    let near  = (getNear plants i)
    match Set.contains near transforms with
    | true -> plant
    | false -> noPlant

let newGeneration transforms plants size =
    let newPlants = newPlants ()
    [-size..size]
    |> Seq.iter (fun i ->
        setPlant newPlants i (transform transforms plants i))
    newPlants
    
let Part1 (input : string) =  // "result1" (*
    let lines = input |> toLines
    let initial = parseInitial lines.[0]
    let transforms = 
        lines.[2..] 
        |> Seq.map parseTansforms 
        |> Seq.filter (fun (_,havePlant) -> havePlant = plant)
        |> Seq.map fst
        |> Set
    let plants = newPlants ()
    initial
    |> Seq.iteri (fun i havePlant -> setPlant plants i havePlant)
    //(print (plants.[180..220] |> String));
    let x = 
        (plants, [1..20])
        ||> Seq.fold (fun plants _ -> 
            let newPlants = newGeneration transforms plants 198
            (print (newPlants.[140..260] |> String));
            newPlants)
    
    plantsValue x


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)