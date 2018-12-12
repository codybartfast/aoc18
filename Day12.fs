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
let maxPot = 10000
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

let checkBounds (plants :char[]) =
    let s = plants.Length
    if plants.[2] = noPlant
        && plants.[s-3] = noPlant
        then ()
        else failwith "Pot Overflow Exception"

////////////////


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
    let plants100 = 
        (plants, [1..100])
        ||> Seq.fold (fun plants _ -> 
            //let oldValue = (plantsValue plants)
            let newPlants = newGeneration transforms plants (maxPot-2)
            checkBounds newPlants
            //let newValue = (plantsValue newPlants)
            //printfn "old:%O, new:%O, diff:%O" oldValue newValue (newValue - oldValue)
            (print (newPlants.[(maxPot-10)..(maxPot+70)] |> String));
            newPlants)
    let value100 = plantsValue plants100 |> int64
    let count100 = plants100 |> Seq.sumBy(function |'.' -> 0L| plant -> 1L)
    count100 
    ((50_000_000_000L - 100L) * (print count100)) + (print value100)


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)