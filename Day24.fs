(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day24

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) |> List.ofArray
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

(* ================ Part A ================ *) 
//[<Measure>]
//type pnt
type Damage = Bludgeoning | Cold | Fire | Radiation | Slashing
type Group = {Army: string; Id: int; Count : int;  HP : int; AD : int; 
                Damage: Damage; Initiative : int;  Weak : Set<Damage>; 
                Immune : Set<Damage> }
type Army =  {Name: string; Groups: Map<int,Group>}
type Armies = Map<string,Army>

let parseDamage = function
    | "bludgeoning" -> Bludgeoning
    | "cold" -> Cold
    | "fire" -> Fire
    | "radiation" -> Radiation
    | "slashing" -> Slashing

let parseGroup army idx line =
    let count, hp, ap, damage, init = 
        line
        |> rxMatch @"(\d+)\D+(\d+)\D+(\d+) (\w+) damage \D+(\d+)"
        |> fun mtch ->
            let grp idx = groupValue mtch idx
            let grpi = grp >> int
            grpi 1, grpi 2, grpi 3, parseDamage (grp 4), grpi 5
    let weak =
        (rxMatch @"weak to ([^;)]+)" line).Groups.[1].Value
        |> rxSplit ", " 
        |> List.map parseDamage  |> Set
    let immune =
        (rxMatch @"immune to ([^;)]+)" line).Groups.[1].Value
        |> rxSplit ", " 
        |> List.map parseDamage  |> Set
    {Army = army; Id = idx; Count = count; HP = hp; AD = ap; 
        Damage = damage; Initiative = init; Weak = weak; 
        Immune = immune}

let parseArmy input =
    let lines = input |> toLines 
    let army = lines.Head.Trim(':')
    let groups = 
        lines.Tail
        |> List.mapi (parseGroup army)
        |> List.map (fun grp -> grp.Id, grp)
        |> Map
    army, {Name = army; Groups = groups}

let parse input =     
    rxSplit "(\r?\n){2,}" input
    |> Seq.map parseArmy
    |> Map

let effectivePower group = group.AD * group.Count

let selectorSortValue group = (effectivePower group, group.Initiative)    

let groupDamage attacker defender =
    let damage = attacker.Damage
    if defender.Immune.Contains damage then 0 else
    let ePower = effectivePower attacker
    if defender.Weak.Contains damage 
        then ePower * 2 else ePower

let targetSortValue attacker defender =
    groupDamage attacker defender,
        effectivePower defender,
            defender.Initiative

let selectTargets attkArmy dfndArmy =
    let attackers = 
        attkArmy.Groups |> Map.toList |> List.map snd
        |> List.sortByDescending selectorSortValue
    let defenders = 
        dfndArmy.Groups |> Map.toList 
        |> List.map snd |> Set
    let _, pairings =
        ((defenders, []), attackers)
        ||> List.fold (fun (defenders, pairings) attacker ->
            let target = 
                if Set.isEmpty defenders then None else
                let (target, delt) = 
                    defenders 
                    |> Seq.map(fun defender -> 
                        defender, groupDamage attacker defender)
                    |> Seq.sortByDescending (fun (dfnd,_) ->
                        targetSortValue attacker dfnd)
                    |> Seq.head
                if delt = 0 then None else Some target
            let defenders = 
                match target with
                | None -> defenders
                | Some trgt -> defenders.Remove trgt            
            defenders, (attacker, target)::pairings)
    pairings
    |> List.choose (fun (attk, dfnd) ->
        match dfnd with
        | Some dfnd -> Some (attk, dfnd)
        | _ -> None)

let groupFight (armies : Armies) ((attkArmy,attkId), (dfndArmy, dfndId)) =
    let attkArmy = armies.[attkArmy]
    let dfndArmy = armies.[dfndArmy]
    match attkArmy.Groups.ContainsKey attkId, 
            dfndArmy.Groups.ContainsKey dfndId with
    | _, false | false, _ -> armies 
    | true, true ->
    let attk = attkArmy.Groups.[attkId]
    if attk.Count <= 0 then armies else
    let dfnd = dfndArmy.Groups.[dfndId]
    let damage = groupDamage attk dfnd
    let unitsLost = damage / dfnd.HP

    let dfnd = {dfnd with Count = dfnd.Count - unitsLost}
    let dfndArmy = 
        if dfnd.Count <= 0 then 
            {dfndArmy with Groups = dfndArmy.Groups.Remove dfnd.Id}
        else
            {dfndArmy with Groups = dfndArmy.Groups.Add (dfnd.Id, dfnd)}
    armies.Add (dfndArmy.Name, dfndArmy)
    

let groupMap (groups: Group list) =
    groups
    |> List.map (fun grp -> grp.Id, grp)
    |> Map

let rec fight (armies : Map<string,Army>) =
    let [(_, army1); (_, army2)] = Map.toList armies

    if army1.Groups.Count = 0 then army2 else
    if army2.Groups.Count = 0 then army1 else

    let pairings = 
        (selectTargets army1 army2) @ (selectTargets army2 army1)
        |> List.sortByDescending (fun (attk, _) -> attk.Initiative)
        |> List.map (fun (attk, dfnd) -> (attk.Army,attk.Id), (dfnd.Army,dfnd.Id))
    let armies = 
        (armies, pairings)
        ||> List.fold (groupFight)
    fight armies

let Part1 (input : string) =  // "result1" (*
    let armies = parse input
    let army = fight armies
    army.Groups
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (fun grp -> grp.Count)
    |> Seq.sum


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)