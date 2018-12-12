(* a.cbf.pub/tx/iUYy2HSEWXsI8cMnj2mgeGj7SOkEOJ0KrT7UkPvNaAU/data.html *)

module Day12

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)

(* ================ Part A ================ *) 

let maxPot = 400
let plant = '#'
let empty = '.'
let hasPlant = function | '#' -> true | _ -> false

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

let newPots () = Array.init (1 + (2 * maxPot)) (fun _ -> empty)

let getPot pots idx =
    Array.item (maxPot + idx) pots

let getNear pots idx range =
    let start = maxPot + idx - range
    let near = Array.sub pots start (range * 2 + 1)
    near |> toString

let setPot (pots : 'a[]) idx value =
    pots.[maxPot + idx] <- value

let valueOfPlants pots =
    pots
    |> Seq.mapi (fun idx pot -> if hasPlant pot then idx - maxPot else 0)
    |> Seq.sum

let transform transforms plants idx =
    let neighbours = (getNear plants idx 2)
    match Set.contains neighbours transforms with
    | true -> plant
    | false -> empty

let transformPots transforms pots =
    let newPots = newPots ()
    [-(maxPot-2)..(maxPot-2)]
    |> Seq.iter (fun idx ->
        setPot newPots idx (transform transforms pots idx))
    newPots

let parseInput input =
    let lines = input |> toLines
    let initial = parseInitial lines.[0]
    let transforms = 
        lines.[2..] 
        |> Seq.map parseTansforms 
        |> Seq.filter (fun (_,havePlant) -> havePlant = plant)
        |> Seq.map fst
        |> Set
    let pots = newPots ()
    initial |> Seq.iteri (fun idx pot -> setPot pots idx pot)   
    (pots, transforms)
    
let Part1 (input : string) =
    let (pots, transforms) = parseInput input
    (pots, [1..20])
        ||> Seq.fold (fun plants _ -> transformPots transforms plants)
    |> valueOfPlants
    
(* ================ Part B ================ *)

let compareGenerations ((_, pots1), (_, pots2)) =
    let firstPlant pots = Array.findIndex ((=) plant) pots
    let lastPlant pots = Array.findIndexBack ((=) plant) pots
    let firstToLast (pots:char[]) = 
        pots.[(firstPlant pots)..(lastPlant pots)]
    if (firstToLast pots1) = (firstToLast pots2) 
    then Some ((firstPlant pots2) - (firstPlant pots1) |> int64)
    else None

let findRepeatingPlantPattern transforms pots = 
    let (Some shift, ((generation, repeatingPlants), _)) = 
        (0, pots)
        |> Seq.unfold (fun (generation, pots) -> 
            let next = (generation + 1, transformPots transforms pots)
            //printfn "%O" (getNear (snd next) 35 37);
            Some (next, next))
        |> Seq.pairwise
        |> Seq.map (fun pair -> (compareGenerations pair, pair))
        |> Seq.find (fun (compareResult, pair) ->
            match compareResult with Some _ -> true | _ -> false)
    (repeatingPlants, generation, shift)

let Part2 result1 (input : string) =  
    // Assumes the pattern of pots eventually repeats and shifts to the 
    // left or to the right with each generation. (Observed by printing 
    // pots to screen.) Presumably input constrained to repeat otherwise 
    // this puzzle would be beyond my ken!
    let (pots, transforms) = parseInput input
    let repeatingPlants, generation, shift = 
        findRepeatingPlantPattern transforms pots
    let currentPlantsValue = valueOfPlants repeatingPlants |> int64
    let plantCount = 
        repeatingPlants |> Seq.sumBy(function |'.' -> 0L | _ -> 1L)
    let remainingGenerations = 50_000_000_000L - (int64 generation)
    currentPlantsValue + ((shift * plantCount) * remainingGenerations)
