(* a.cbf.pub/tx/GiX3h3VbJv-E7abYOKxkGt-DeRRTmdD-AgyaY-YQQDM/data.html *)

module Day07

open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

(* ================ Part A ================ *) 

type Node = string * (string list)

let parseLine  = rxMatch "S.*([A-Z]).*([A-Z])" >> fun mtch ->
    let grp idx = groupValue mtch idx
    grp 1, grp 2

let getIndependantsAndAll data =
    let followers = data |> Seq.map snd |> Set
    let befores =  data |> Seq.map fst |> Set
    let independants = (Set.difference befores followers) 
    let all = Set.union befores followers
    (independants, all)

let dependencyChecker data all =
    let findDependencies step =
        (data |> Seq.filter (fun (_, f) -> f = step) |> Seq.map fst |> Set)
    let dependencyMap =
        all |> Seq.map (fun step -> step, findDependencies step)  |> Map
    (fun completedSteps step ->
        Set.difference (dependencyMap.[step]) (Set completedSteps) 
        |> Set.isEmpty)    
    
let rec findPath checkEpendencies finished (toDo : string Set) available =
    if toDo.IsEmpty then List.rev finished 
    else
        let step = available |> Seq.sort |> Seq.head
        let finished' = step::finished
        let toDo' = toDo.Remove step
        let available' = 
            toDo' |> Seq.filter (checkEpendencies finished') |> Set
        findPath checkEpendencies finished' toDo' available'

let Part1 input = 
    let data = input |> toLines |> List.map parseLine
    let (start, all) = getIndependantsAndAll data

    findPath (dependencyChecker data all) [] all (Set start)
    |> String.concat ""


(* ================ Part B ================ *)

type Schedule = Map<int,string list>

let doWork toDo elves checkDependencies =

    let pickTask finished toDo = 
        let steps = 
            toDo
            |> Seq.filter (checkDependencies finished) 
            |> Seq.sort 
        if Seq.isEmpty steps then None
        else
            let step = Seq.head steps
            Some (step, step |> Seq.head |> int |> (fun a -> a - 4)) 

    let rec startTasks finished toDo (schedule : Schedule) elves time =
        match elves, pickTask finished toDo with
        | 0, _ | _, None ->  (toDo, schedule, elves) 
        | _, Some (step, duration) ->
            let finishTime = time + duration
            let alreadyScheduled = 
                match schedule.ContainsKey finishTime with
                | false -> []
                | true -> schedule.[finishTime]
            startTasks 
                finished 
                (Set.remove step toDo) 
                (schedule.Add(finishTime, step::alreadyScheduled))
                (elves - 1)
                time 

    let rec advanceSchedule finished toDo (schedule : Schedule) elves time =
        match (Set.count toDo), schedule.Count with
        | 0, 0 -> time, finished
        | _ ->
            let (time, justFinished) = schedule |> Map.toSeq |> Seq.minBy fst
            let currentSchedule = schedule.Remove time
            let finished' = (justFinished |> List.sort) @ finished
            let (toDo', schedule', elves') = 
                startTasks finished' toDo currentSchedule (elves + 1) time
            advanceSchedule finished' toDo' schedule' elves' time
    
    let schedule = (Map [(0, [])])
    advanceSchedule [] toDo schedule elves 0

let Part2 result1 input = 
    let data = input |> toLines |> List.map parseLine
    let (_ , all) = getIndependantsAndAll data
    doWork all 5 (dependencyChecker data all) |> fst
