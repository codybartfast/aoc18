(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day04

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

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
type State = Awake | Asleep
let rxIsMatch pattern str = Regex.IsMatch(str, pattern)

//          1         
//012345678901234567890123456789012345678901
//[1518-11-16 00:00] Guard #2039 begins shift
let getTime (str:string) = 
    DateTime.Parse(str.Substring(1, 11)+"T"+str.Substring(12,5))

let analyseShift  (map : Map<string, Map<int, int>>) (timeEntries : (DateTime * string) list) =
    let entries =
        timeEntries
        |> List.map (fun (time, str) -> (time.Hour, time.Minute, str))
    //let startMinute  = 
    //    let time = timeEntries.Head |> fst
    //    match time.Hour, time.Minute with
    //    | 0, m -> m
    //    | _ -> -1

    let entriesWithEnd = entries @ [(0, 61, "any string")]

    (((-1000, "null guard", Awake), map), entriesWithEnd)
    ||> List.fold (fun ((prevStart, prevGuard, prevState), map) (hour, minute, text) -> 

        let thisStart = 
            match hour, minute with
            | 0, m -> m
            | _ -> -1                

        let map = 
            match prevState with
            | Asleep ->
                (map, [prevStart..(thisStart-1)])
                ||> List.fold (fun map min -> 
                    let minuteMap = 
                        match map.TryGetValue prevGuard with
                        | (true, m) -> m
                        | _ -> Map.empty
                    let count =
                        match minuteMap.TryGetValue min with
                        | (true, c) -> c
                        | _ -> 0
                    let newMinuteMap = minuteMap.Add (min, (count + 1))
                    map.Add (prevGuard, newMinuteMap)  
                )
            | Awake -> map  
        let thisGuard = 
            rxMatch "\#(\d+)" text |> fun m ->
                if m.Success then groupValue m 1 else prevGuard

        let thisState =
            if text.Contains("asleep") then Asleep else Awake
        ((thisStart, thisGuard, thisState), map))
    |> snd

let Part1 input = 
    let map =
        input |> toLines
        |> List.map (fun str -> ((getTime str), str))
        |> List.groupBy (fun (time, _) -> time.AddHours(1.0).DayOfYear)
        |> List.map (snd >> List.sortBy fst)
        |> List.fold analyseShift (Map.empty)
    
    let (guard, minute, _) =
        map
        |> Map.toSeq
        |> Seq.map (fun (guard, minMap) ->
            let (freqMin, freqTotal) =
                minMap
                |> Map.toSeq
                |> Seq.maxBy snd
            guard, freqMin, freqTotal)
        |> Seq.maxBy (fun (_, _, freqTotal) -> freqTotal)

    (guard |> int) * minute

(* ================ Part B ================ *)

let Part2 result1 input = 
    "result2"
