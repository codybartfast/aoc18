(* a.cbf.pub/tx/RBjS1GCJ-eLKtId1j8XamdAFh3m8ae6QuN-AV1hLWW8/data.html *)

module Day15Redux

// #nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    //|> List.ofArray
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
let toHex = 
    BitConverter.ToString >> (fun str -> str.Replace("-", String.Empty))
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

type Race = Elf | Goblin

[<Measure>]
type points

type Unit = {Race : Race; AP : int<points>;  HP : int<points>}
type Square = Wall | Open | Unit of Unit
type Cavern = Square[,]
type Location = int*int
type Path = Location list
type UnitLocation = Unit*Location

type TurnResult = OnGoing of Cavern | Over of Cavern

let to2D rows =
    let Y = Array.length rows
    let X = Array.length rows.[0]
    Array2D.init X Y (fun x y -> rows.[y].[x])

let toRows array2d =
    let X = Array2D.length1 array2d
    let Y = Array2D.length2 array2d
    Array.ofSeq 
        (seq{for y in seq{0..Y-1}
           do yield Array.ofSeq 
                (seq{for x in seq{0..X-1} do yield array2d.[x,y]})})

let square elfAP goblinAP startHP = function
    | '#' -> Wall
    | '.' -> Open
    | 'G' -> Unit {Race = Goblin; AP = goblinAP; HP = startHP}
    | 'E' -> Unit {Race = Elf; AP = elfAP; HP = startHP}
    | _ -> failwith "oops!"

let charOfSqaure = function
    | Wall -> '█'
    | Open -> '.'
    | Unit {Race = Elf} -> 'E'
    | Unit {Race = Goblin} -> 'G'

let strOfSquare = function
    | Wall -> "████"
    | Open -> "    "
    | Unit unit ->
        match unit.Race with
        | Elf -> "E"+unit.HP.ToString().PadLeft(3)
        | Goblin -> "G"+unit.HP.ToString().PadLeft(3)

let parse goblinAP elfAP startHP input =
    input 
    |> toLines
    |> Array.map (toChars >> Array.map (square goblinAP elfAP startHP))
    |> to2D

let display cavern =
    printfn ""
    cavern
    |> toRows
    |> Array.map (Array.map strOfSquare >> (String.concat ""))
    |> Array.iter (printfn "%s")
    cavern

let allLocations (cavern:Cavern) =
    seq{for y in seq{cavern.GetLowerBound(1)..cavern.GetUpperBound(1)} do
        for x in seq{cavern.GetLowerBound(0)..cavern.GetUpperBound(0)} do
        yield (x,y)}  

let distance ((_,loc1):UnitLocation) ((_,loc2):UnitLocation) =
    let (x, y), (x', y') = loc1, loc2
    abs (x - x') + abs (y - y')

let adjacent unit1 unit2 = distance unit1 unit2 = 1

let sortValue (loc:Location) =
    let x,y = loc
    (y,x)
    
let get (cavern:Cavern) (loc:Location) =
    let x,y = loc
    cavern.[x,y]

let set (cavern:Cavern) (loc:Location) square =
    let x,y = loc
    cavern.[x,y] <- square
    cavern

let isOpen cavern loc =
    match get cavern loc with
    | Open -> true
    | _ -> false

let adjacentOpen (cavern:Cavern) loc =
    let x,y = loc
    [ x,y-1; x-1,y; x+1,y; x,y+1 ]
    |> Seq.choose (fun loc ->
        match get cavern loc with
        | Open -> Some loc
        | _ -> None)
    |> Set

let getUnit (cavern:Cavern) (loc:Location) = 
    match get cavern loc with
    | Unit unit -> Some (unit, loc)
    | Open -> None
    | _ -> failwith "oops"

let race ({Race = race}:Unit) = race
let attackPoints ({AP = ap}:Unit) = ap
let hitPoints ({HP = hp}:Unit) = hp
let isAlive (unit:Unit) = hitPoints unit >= 0<points>

let getUnits (cavern:Cavern) : UnitLocation list =
    (allLocations cavern)
    |> Seq.choose (fun loc ->
        match get cavern loc with
        | Wall -> None
        | Open -> None
        | Unit unit when isAlive unit -> Some (unit, loc)
        | _ -> failwith "oops!")
    |> List.ofSeq

let getUnitLocations cavern =
    getUnits cavern |> List.map snd

let getEnemies (cavern:Cavern) ((unit,_):UnitLocation) : UnitLocation list =
    let unitRace = race unit
    getUnits cavern
    |> List.filter (fun (other,_) -> race other <> unitRace)

let chooseAdjacentEnemy unit enemies =
    let adjacent =
        enemies
        |> List.filter (fun enemy -> adjacent unit enemy)
        |> List.sortBy (fun (unit, loc) -> (hitPoints unit, sortValue loc))
    match adjacent.IsEmpty with
    | true ->  None
    | _ -> Some adjacent.Head

let attack (cavern:Cavern) ((unit,unitLoc):UnitLocation) ((enemy,enemyLoc):UnitLocation) : Cavern =
    let {AP = ap} = unit
    let {HP = hp} = enemy
    let newHP = hp - ap
    if newHP > 0<points> then
        let damagedEnemy = {enemy with HP = newHP}
        set cavern enemyLoc (Unit damagedEnemy)
    else
        set cavern enemyLoc Open        

let step (cavern:Cavern) ((unit,loc):UnitLocation) (newLoc:Location) : Cavern * UnitLocation =
    if not (isOpen cavern newLoc) then failwith "oops!"
    let cavern = set cavern loc Open
    let cavern = set cavern newLoc (Unit unit)
    cavern, (unit, newLoc)

let inRange (cavern:Cavern) (enemyLocations:Location list) : Set<Location> = 
    enemyLocations
    |> Seq.map (adjacentOpen cavern)
    |> Set.unionMany

    
let reachable (cavern:Cavern) (loc:Location) : Set<Location*int> =

    let rec expand (knownWithDist:Set<(Location*int)>) (foundWithDist : Set<(Location*int)>) =
        if foundWithDist.IsEmpty then knownWithDist else

        let knownWithDist = Set.union knownWithDist foundWithDist
        let known = knownWithDist |> Set.map fst
        let newFoundWithDist = 
            foundWithDist
            |> Seq.map (fun (loc, distance) ->
                let adjacent = adjacentOpen cavern loc
                Set.difference  adjacent known
                |> Set.map (fun loc' -> (loc', distance + 1)))
            |> Seq.reduce Set.union
        expand knownWithDist newFoundWithDist
    let xxx = expand Set.empty (Set.singleton (loc, 0)) ////////////////
    xxx

let chooseDestination cavern loc (destinations:Set<Location>) =
    if destinations.IsEmpty then None else

    let reachableDestinations = 
        reachable cavern loc
        |> Set.filter (fun (loc, _) ->
            destinations.Contains loc)
    if reachableDestinations.IsEmpty then None else
    reachableDestinations
    |> Seq.groupBy snd
    |> Seq.sortBy fst
    |> Seq.head
    |> snd
    |> Seq.map fst
    |> Seq.sortBy sortValue
    |> Seq.cache
    |> fun destinations ->
        if Seq.isEmpty destinations then None else
        Some (Seq.head destinations)

let findNextLocation  (cavern:Cavern) (loc:Location) (enemyLocations:Location list) : Location option  =
    let destination = chooseDestination cavern loc  (inRange cavern enemyLocations) 
    match destination with
    | None -> None
    | Some destination ->
    chooseDestination cavern destination (adjacentOpen cavern loc)
    
let move (cavern:Cavern) ((unit,loc):UnitLocation) (enemies:UnitLocation list) : Cavern * UnitLocation =
    let next = findNextLocation cavern loc (enemies |> List.map snd)
    match next with
    | None -> cavern, (unit,loc)
    | Some nextLoc -> step cavern (unit,loc) nextLoc

let turn turnResult (loc:Location) : TurnResult = 
    //match turnResult with
    //| OnGoing cavern -> display cavern |> ignore
    //| Over cavern -> display cavern |> ignore
    ////Console.ReadKey()

    match turnResult with
    | Over cavern -> Over cavern
    | OnGoing cavern -> 
    match getUnit cavern loc with
    | None -> turnResult 
    | Some unit ->

    let enemies = getEnemies cavern unit

    if enemies.IsEmpty then Over cavern else
    match chooseAdjacentEnemy unit enemies with
    | Some enemy  -> OnGoing (attack cavern unit enemy)
    | _ ->
        let cavern, unit = move cavern unit enemies
        match chooseAdjacentEnemy unit enemies with
        | Some enemy  -> OnGoing (attack cavern unit enemy)
        | _ -> OnGoing cavern

let round gameResult = 
    match gameResult with
    | Over _ -> gameResult
    | OnGoing cavern -> 
        display cavern
        (gameResult, getUnitLocations cavern)
        ||> List.fold turn

let battle cavern =
    ((OnGoing cavern), 0)
    |> Seq.unfold (fun (result, count) -> 
        Some ((result, count), (round result, count + 1)))

let outcome (cavern, round) =
    let points =
        cavern 
        |> getUnits
        |> Seq.sumBy (fst >> fun {HP = hp} -> hp)
    printfn "(%i x %i)" round points
    (points * round)

let Part1 =
    (parse 15<points> 3<points> 200<points>)
    >> battle
    >> Seq.pick(fun (result, round) ->
        match result with
        | (OnGoing _) -> None
        | (Over cavern) -> Some ((display cavern), round - 1))
    >> outcome

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)