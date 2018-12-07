(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day07

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Drawing

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let rec repeat item = seq{ yield item; yield! repeat item }
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let toString (chrs : seq<char>) = String(Array.ofSeq chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

type Node = string * (string list)

let parseLine  = rxMatch "S.*([A-Z]).*([A-Z])" >> fun mtch ->
    let grp idx = groupValue mtch idx
    let grpi = grp >> int
    grp 1, grp 2

let getEnds data =
    let followers = data |> Seq.map snd |> Set
    let befores =  data |> Seq.map fst |> Set
    let start = (Set.difference befores followers) 
    let finish = (Set.difference followers befores) |> Seq.exactlyOne
    let all = Set.union befores followers
    (start, finish, all)

let getDeps (data : (string*string) list) all =
    all
    |> Seq.map (fun inst -> inst, (data |> Seq.filter (fun (b,f) -> f = inst) |> Seq.map fst |> Set))
    |> Map
    
let rec findPath (deps : Map<string,Set<string>>) (soFar : string list)  (toDo : string Set) (available : string Set) =
    if available.IsEmpty then List.rev soFar else
    let instr = available |> Seq.sort |> Seq.head
    let soFar' = instr::soFar
    let toDo' = toDo.Remove instr
    let available' = 
        toDo'
        |> Seq.filter (fun t -> Set.difference deps.[t] (Set soFar') = Set.empty) 
        |> Set
    findPath deps soFar' toDo' available'
  
let Part1 (input : string) =    "result1" (*
    let data =
        input |> toLines |> List.map parseLine
    let (start, finish, all) = getEnds data

    findPath (getDeps data all) [] all (Set start)
    |> String.concat ""


//*)

    

(* ================ Part B ================ *)

let pickTask (deps : Map<string,Set<string>>) soFar toDo = 
    let instrs = 
        toDo
        |> Seq.filter (fun t -> Set.difference deps.[t] (Set soFar) = Set.empty) 
        |> Seq.sort |> List.ofSeq
    match instrs with
    | [] -> None
    | instr::tail -> 
        Some (instr, instr |> Seq.head |> int |> (fun a -> a - 4)) // <<<<<<--------  64 vs 4


let rec startTasks deps soFar toDo (schedule : Map<int,string list>) elves time =
    if elves = 0 then (toDo, schedule, elves) else

    match pickTask deps soFar toDo with
    | None -> (toDo, schedule, elves)
    | Some (instr, duration) ->
        let finish = time + duration
        let completing = 
            match schedule.ContainsKey finish with
            | false -> [instr]
            | true -> instr::(schedule.[finish])
        startTasks 
            deps
            soFar 
            (Set.remove instr toDo) 
            (schedule.Add(finish, completing))
            (elves - 1)
            time 

let rec assign (deps : Map<string,Set<string>>) (soFar : string list) (toDo : string Set) (schedule : Map<int,string list>) (elves : int)  time =
    
    match toDo.Count, schedule.Count with
    | 0, 0 -> time, soFar
    | _ ->
    

    let (time, completed) = schedule |> Map.toSeq |> Seq.minBy fst
    let currentSchedule = schedule.Remove time
    let soFar' = (completed |> List.sort) @ soFar
    let (toDo', schedule', elves') = startTasks deps soFar' toDo currentSchedule (elves + 1) time
    assign deps soFar' toDo' schedule' elves' time
  

let Part2 result1 (input : string) =  // "result2" (*
    let data =
        input |> toLines |> List.map parseLine
    let (start, finish, all) = getEnds data

    let (time, path) = assign (getDeps data all) [] all  (Map [(0, [] )]) 5 0 // <<<<<<-------- 2 vs 5
    (time, path |> List.rev |> String.concat "")





//*)