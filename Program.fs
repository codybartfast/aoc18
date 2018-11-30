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
let readLines name = File.ReadAllLines(getInputFile name)
let readText name = File.ReadAllText(getInputFile name)

open Day01
let name = "day01"

let text = readText name
let lines = readLines name

[<EntryPoint>]
let main argv =

    let inputA = lines
    
    sw.Start()
    let resultA = PartA inputA
    printfn "%O" resultA 
    showTime ()

    let inputB = inputA

    sw.Restart() 
    let resultB = PartB resultA inputB
    printfn "%O" resultB
    showTime ()

    Console.ReadKey() |> ignore
    0
