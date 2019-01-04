(* a.cbf.pub/tx/Qckma-96sYFdqwA-KIM66DCWb1Of-7Odcp2jUa1pyXI/data.html *)

module Day24

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) |> List.ofArray

(* ================ Part A ================ *) 

type Damage = Bludgeoning | Cold | Fire | Radiation | Slashing
type Group = {Army: string; Id: int; Count : int;  HP : int; AD : int; 
                Damage: Damage; Initiative : int;  Weak : Set<Damage>; 
                Immune : Set<Damage> }
type Army =  {Name: string; Groups: Map<int,Group>}
type Armies = Map<string,Army>
type Aftermath  = Victory of Army | Stalemate

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
                let (target, damage) = 
                    defenders 
                    |> Seq.map(fun defender -> 
                        defender, groupDamage attacker defender)
                    |> Seq.sortByDescending (fun (dfnd,_) ->
                        targetSortValue attacker dfnd)
                    |> Seq.head
                if damage = 0 then None else Some target
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

let groupFight (armies : Armies) pairing =
    let ((atkArmyId, atkGroupId), (defArmyId, defGroupId)) = pairing
    let atkArmy = armies.[atkArmyId]
    let defArmy = armies.[defArmyId]
    if not (atkArmy.Groups.ContainsKey atkGroupId
            && defArmy.Groups.ContainsKey defGroupId) 
    then armies 
    else
        let atkGroup = atkArmy.Groups.[atkGroupId]
        if atkGroup.Count <= 0 then armies else
        let defGroup = defArmy.Groups.[defGroupId]
        let damage = groupDamage atkGroup defGroup
        let unitsLost = damage / defGroup.HP

        let defGroup' = {defGroup with Count = defGroup.Count - unitsLost}
        let defArmy' = 
            if defGroup'.Count <= 0 then 
                {defArmy with Groups = defArmy.Groups.Remove defGroup'.Id}
            else
                {defArmy with 
                    Groups = defArmy.Groups.Add (defGroup'.Id, defGroup')}
        armies.Add (defArmy'.Name, defArmy')

let groupMap (groups: Group list) =
    groups
    |> List.map (fun grp -> grp.Id, grp)
    |> Map

let rec fight (armies : Map<string,Army>)  =
    let [(_, army1); (_, army2)] = Map.toList armies

    if army1.Groups.Count = 0 then Victory army2 else
    if army2.Groups.Count = 0 then Victory army1 else

    let pairings = 
        (selectTargets army1 army2) @ (selectTargets army2 army1)
        |> List.sortByDescending (fun (atk, _) -> atk.Initiative)
        |> List.map (fun (atk, def) -> 
            (atk.Army, atk.Id), (def.Army, def.Id))
    let veteranArmies = 
        (armies, pairings)
        ||> List.fold (groupFight)
    if armies = veteranArmies 
        then Stalemate
        else fight veteranArmies

let unitCount army = 
    army.Groups
    |> Map.toSeq
    |> Seq.sumBy (fun (_, grp) -> grp.Count)

let Part1 (input : string) =
    let armies = parse input
    let (Victory army) = fight armies
    unitCount army

(* ================ Part B ================ *)

let immune = "Immune System"

let boostImmune boost (armies : Armies) =
    let army = armies.[immune]
    let groups  =
        army.Groups
        |> Map.toList
        |> List.map 
            (fun (id, grp) -> id, {grp with AD = grp.AD + boost})
        |> Map
    armies.Add (immune, {army with Groups = groups})

let Part2 result1 (input : string) = 
    let armies = parse input
    let rec boostToVictory boost =  
        let armies = boostImmune boost armies 
        match fight armies with
        | Victory army when army.Name = immune -> unitCount army
        | _ -> boostToVictory (boost + 1)
    boostToVictory 1