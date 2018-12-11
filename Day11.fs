(* a.cbf.pub/tx/SS9Blb0dvnqdcdQ5waGIIoU-u47AsBnGCPQgTIeYCv4/data.html *)

module Day11

(* ================ Part A ================ *) 

let gridSize = 300

let getCellLevel serial =
    let calc (x,y) = 
        let rack = x + 10
        ((((rack * y) + serial) * rack % 1000) / 100) - 5    
    let powerGrid = 
        [|1..gridSize|]
        |> Array.map (fun y -> 
            [|1..gridSize|] |> Array.map (fun x -> calc (x,y)))
    (fun (x,y) -> powerGrid.[y-1].[x-1])

let allSquares = seq{ 
    for x in seq{1..gridSize} do
    for y in seq{1..gridSize} do
        yield (x,y)}

let threeSquare (x,y) =
    [for x in x..(min gridSize (x+2)) -> 
        [for y in y..(min gridSize (y+2)) -> (x,y)]]
    |> Seq.collect id

let Part1 (input : string) = 
    let serial = input |> int
    let cellLevel = getCellLevel serial
    allSquares
    |> Seq.filter (fun (x,y) -> x < (gridSize - 1) && y < (gridSize - 1))
    |> Seq.map (fun corner -> 
        (corner,  corner |> threeSquare |> Seq.sumBy cellLevel))
    |> Seq.maxBy snd
    |> fst

(* ================ Part B ================ *)

let evalSquaresStartingWith cellLevel (left, top) = 
    let rec evalAndExpand prevLevel (right, bottom) = seq{
        yield ((left, top, (right-left)), prevLevel)
        if right <= gridSize && bottom <= gridSize then
            let rightEdgeLevel = 
                seq{for y in top..bottom -> (right,y)} 
                |> Seq.sumBy cellLevel
            let bottomEdgeLevel = 
                seq{for x in left..(right-1) -> (x,bottom)} 
                |> Seq.sumBy cellLevel
            let newLevel =  
                prevLevel + rightEdgeLevel + bottomEdgeLevel
            yield! evalAndExpand newLevel (right+1, bottom+1)}
    let cornerLevel = cellLevel (left, top)
    evalAndExpand cornerLevel (left+1, top+1)

let Part2 result1 (input : string) = // slow :(
    let serial = input |> int
    let cellLevel = getCellLevel serial
    allSquares
    |> Seq.collect (evalSquaresStartingWith cellLevel)
    |> Seq.maxBy snd
    |> fst
