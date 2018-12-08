(* a.cbf.pub/tx/O4E39PY71rUtmkOLkXFBvFhYOoH9f64V_NzUCK3ebrM/data.html *)

module Day08

#nowarn "0025"

(* ================ Part A ================ *) 

let parse (input:string) = 
    input.Split(' ') |> List.ofArray |> List.map (int)

let readMetadata (data, mdSum) mdCount =
    (data |> List.skip mdCount)
    , mdSum + List.sum (data |> List.take mdCount)

let rec readChild (data, mdTotal) =
    match data with
    | chCount::(mdCount::tail) -> 
        let (data, mdTotal) =
            ((tail, mdTotal), [1..chCount])
            ||> List.fold (fun state _ -> readChild state)
        readMetadata (data, mdTotal) mdCount

let Part1 (input : string) =
    readChild (parse input, 0) |> snd
    

(* ================ Part B ================ *)

let readMetadataValue data mdCount childValues =
    let metadata = List.take mdCount data
    let childCount = Array.length childValues
    let value = 
        if childCount = 0 then
             List.sum metadata
         else 
            metadata
            |> List.filter ((>=) childCount)
            |> List.sumBy (fun idx -> childValues.[idx  - 1])
    value, (List.skip mdCount data)

let rec readValue data  =
    match data with
    | chCount::(mdCount::tail) -> 
        let (values, data) =
            (tail, [1..chCount])
            ||> List.mapFold (fun data _ -> (readValue data))
        readMetadataValue data mdCount (values |> Array.ofList)

let Part2 result1 (input : string) =
    readValue (parse input) |> fst
