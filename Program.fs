open System
open System.IO
open System.Diagnostics

printfn ""
let sw = Stopwatch ()
let showTime () = 
    printfn "(%s seconds)" (sw.Elapsed.TotalSeconds.ToString("n2"))
    printfn ""

let getInputFile name =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", name) + ".txt"
let readText name = File.ReadAllText(getInputFile name)

open Day20
let name = "day20"

let text = readText name

[<EntryPoint>]
let main argv =

    let input1 = text
    
    sw.Start()
    let result1 = Part1 input1
    //printfn "%A" result1 
    printfn "%O" result1 
    showTime ()

    let input2 = input1

    sw.Restart() 
    let result2 = Part2 result1 input2
    printfn "%A" result2
    printfn "%O" result2
    showTime ()

    Console.ReadKey() |> ignore
    0
