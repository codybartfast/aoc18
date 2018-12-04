(* a.cbf.pub/tx/HKWGUqMmKYd7HsfzxwRdM4rdHIEIz_q8VWqHuaa8knk/data.html *)

module Day04

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

(* ================ Part A ================ *) 

type State = Awake | Asleep
type GuardMap = Map<string, Map<int, int>>

let parseLine (line:string) =
    let time =  DateTime.Parse(line.Substring(1, 16))
    let guard = 
        let mtch = rxMatch "\#(\d+)" line
        if mtch.Success then Some (groupValue mtch 1) else None
    let state = 
        if line.Contains("asleep") then Asleep else Awake
    (time, (guard, state))

let buildGuardMap = 
    let addMinute guard (map : GuardMap) minute  =
        let minuteMap = 
            match map.TryFind guard with
            | Some minuteMap -> minuteMap | _ -> Map.empty
        let count =
            match minuteMap.TryGetValue minute with
            | (true, count) -> count  | _ -> 0
        map.Add (guard, minuteMap.Add (minute, (count + 1)))

    let addEntry (previousInfo, map) entry =
        let (prevMinute, prevGuard, prevState) = previousInfo
        let (minute, (guardOpt, state)) = entry
        let guard = 
            match guardOpt with 
            | Some guard -> guard 
            | _ -> prevGuard
        let map = 
            match prevState with
            | Awake -> map  
            | Asleep ->
                Seq.fold (addMinute prevGuard) map [prevMinute..(minute-1)]
        ((minute, guard, state), map)

    let addEntriesForDay map =
        Seq.sortBy fst
        >> Seq.map (fun ((time : DateTime), data) -> (time.Minute, data))
        >> fun entries -> seq{ yield! entries; yield (61, (None, Awake))}           
        >> Seq.fold addEntry ((0, "any string", Awake), map)
        >> snd

    Seq.map parseLine    
    >> Seq.groupBy (fun (time, _) -> time.AddHours(11.0).DayOfYear)
    >> Seq.map snd
    >> Seq.fold addEntriesForDay Map.empty

let Part1 input = 
    let guardMap = input |> toLines |> buildGuardMap
    let sleepyGuard =
        guardMap
        |> Map.toSeq
        |> Seq.groupBy fst
        |> Seq.maxBy(fun (_, minuteMap) ->
                minuteMap
                |> Seq.collect (snd >> Map.toSeq)
                |> Seq.sumBy snd)
        |> fst 
    let sleepyMinute = 
        guardMap.[sleepyGuard]
        |> Map.toSeq
        |> Seq.maxBy snd
        |> fst
    (sleepyGuard |> int) * sleepyMinute
    
(* ================ Part B ================ *)

let Part2 result1 input = 
    input |> toLines |> buildGuardMap
    |> Map.toSeq
    |> Seq.map (fun (guard, minuteMap) ->
        minuteMap
        |> Map.toSeq
        |> Seq.maxBy snd
        |> fun (freqMinute, freqCount) -> guard, freqMinute, freqCount)
    |> Seq.maxBy (fun (_, _, freqTotal) -> freqTotal)
    |> fun (freqGuard, freqMinute, _) -> (freqGuard |> int) * freqMinute
