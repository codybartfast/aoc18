(* a.cbf.pub/tx/LzH9XaNqhGE4ZmW4ENWEUgKCChO3J4EDrf8y_Kbgr8g/data.html *)

module Day14

#nowarn "0025"

open System

(* ================ Part A ================ *) 

type Chart = ((int[] * int) * (int * int))

let newChart () =
    let scores = Array.zeroCreate 1024
    scores.[0] <- 3
    scores.[1] <- 7
    ((scores, 2), (0, 1))

let inline getDigits quality =
    match quality < 10 with
    | true -> None, quality
    | false -> Some 1, quality % 10

let inline addNextRecipes (chart : Chart) = 
    let (scores, size), (elf1, elf2) = chart
    let scores =
        if size + 2 >= scores.Length then
            let bigger = Array.zeroCreate (scores.Length * 2)
            Array.blit scores 0 bigger 0 size
            bigger
        else scores
    let size =           
        match getDigits (scores.[elf1] + scores.[elf2]) with
        | None, quality -> 
            scores.[size] <- quality
            size + 1
        | Some 1, quality -> 
            scores.[size] <- 1
            scores.[size + 1] <- quality
            size + 2
    let moveElf elf = (1 + (elf + scores.[elf])) % (size)
    ((scores, size), (moveElf elf1, moveElf elf2))

let rec createRecipes chart =
    seq{yield chart
        yield! createRecipes (addNextRecipes chart)}

let Part1 (input : string) =  
    let threshold = int input        
    let ((scores, _), _) = 
        createRecipes (newChart ())
        |> Seq.find (fun ((_, size), _) -> size >= threshold + 10)
    let result = 
        scores.[threshold .. (threshold + 9)]
        |> Seq.map(fun i -> i.ToString()) |> String.concat ""   
    result

(* ================ Part B ================ *)

let cookieDigits cookie =
    let rec handleLeast number digits =
        match number > 0 with
        | false -> digits
        | true ->
            handleLeast (number / 10) ((number % 10)::digits)
    handleLeast cookie [] |> Array.ofList

let inline isCookie cookie scores idx =
    let cSpan = ReadOnlySpan(cookie)
    let span = ReadOnlySpan(scores, idx, cookie.Length)
    cSpan.SequenceEqual(span)

let Part2 result1 (input : string) = 
     let cookieNum = int input
     let cookie = cookieDigits cookieNum
     let firstDigit = cookie.[0]
     let cookieSize = cookie.Length
     
     createRecipes (newChart ())
     |> Seq.skip (cookieSize + 1)
     |> Seq.find (fun ((scores, size), _) ->
        let idx1 = size - (cookieSize + 1)
        let idx2 = size - cookieSize
        ((scores.[idx1] = firstDigit && isCookie cookie scores idx1)
            || (scores.[idx2] = firstDigit && isCookie cookie scores idx2)))
     |> fun ((scores, size), _ ) ->
            let idx = size - (cookieSize + 1)
            match isCookie cookie scores idx with
            | true -> idx
            | false -> idx + 1
