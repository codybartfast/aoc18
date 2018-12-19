(* a.cbf.pub/tx/Xd1_W_Jh1_-uF6j-ORl6YFF7InrRlpPqkU3sclrytmM/data.html *)

module Day19

#nowarn "0025"

open System
open System.Text.RegularExpressions

let getLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

let print obj = (printfn "%A" obj); obj

(* ================ Part A ================ *) 

type Instruction = string * int * int * int

let parseLine line : Instruction = 
    line
    |> rxMatch "([a-z]+) (\d+) (\d+) (\d+)" 
    |> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        (grp 1, grpi 2, grpi 3, grpi 4)

let parse input =
    let lines = input |> getLines
    let ipReg = lines.[0] |> rxMatch "\d" |> (fun m -> m.Value) |> int
    let program = lines |> Array.skip 1 |> Array.map parseLine
    (program, ipReg)

let run (program : Instruction[]) registers ipReg solve  =
    let toInt = (function true -> 1 | false -> 0)
    let rec tick (reg :int[]) ip =
        match solve reg with
        | Some ans -> ans
        | None -> 
        reg.[ipReg] <- ip 
        match program.[ip] with
        | ("addr", A, B, C) -> reg.[C] <- (reg.[A] + reg.[B]);
        | ("addi", A, B, C) -> reg.[C] <- (reg.[A] + B);
        | ("mulr", A, B, C) -> reg.[C] <- (reg.[A] * reg.[B]);
        | ("muli", A, B, C) -> reg.[C] <- (reg.[A] * B);
        | ("banr", A, B, C) -> reg.[C] <- (reg.[A] &&& reg.[B]);
        | ("bani", A, B, C) -> reg.[C] <- (reg.[A] &&& B);
        | ("borr", A, B, C) -> reg.[C] <- (reg.[A] ||| reg.[B]);
        | ("bori", A, B, C) -> reg.[C] <- (reg.[A] ||| B);
        | ("setr", A, B, C) -> reg.[C] <- (reg.[A]);
        | ("seti", A, B, C) -> reg.[C] <- A;
        | ("gtir", A, B, C) -> reg.[C] <- ((A > reg.[B]) |> toInt);
        | ("gtri", A, B, C) -> reg.[C] <- ((reg.[A] > B) |> toInt);
        | ("gtrr", A, B, C) -> reg.[C] <- ((reg.[A] > reg.[B]) |> toInt);
        | ("eqir", A, B, C) -> reg.[C] <- ((A = reg.[B]) |> toInt);
        | ("eqri", A, B, C) -> reg.[C] <- ((reg.[A] = B) |> toInt);
        | ("eqrr", A, B, C) -> reg.[C] <- ((reg.[A] = reg.[B]) |> toInt);
        tick reg (reg.[ipReg] + 1)
    tick registers 0

let Part1 (input : string) = 
    let (program, ipReg) = parse input 
    
    let solve (reg:int[])  =
        if reg.[ipReg] > program.Length
        then Some reg.[0]
        else None

    run program (Array.zeroCreate 6) ipReg solve
     
(* ================ Part B ================ *)

let Part2 result1 (input : string) = // "result2" (*
    let (program, ipReg) = parse input 
    
    let limit = 100
    let mutable captures = List.empty
    let solve (reg:int[])  =
        if captures.Length < limit 
        then captures <- (reg |> List.ofArray)::captures; None
        else Some 0

    let registers = Array.zeroCreate 6
    registers.[0] <- 1
    run program registers ipReg solve |> ignore
    
    let sampleSize = 40
    let sample = captures |> List.take sampleSize |> List.rev
    let reg2 =
        sample 
        |> List.map (fun r -> r.[2]) 
        |> List.distinct 
        |> List.exactlyOne

    let ipRegs =
        sample
        |> List.map (fun r -> r.[ipReg]) 
        |> List.distinct 
        |> List.sort

    printfn "After a few dozen cycles a fairly stead pattern is seen:"
    printfn "   Registry 2 settles to: %i" reg2
    printfn "   The IP registry sees a small range of values:"
    printfn "       %A" ipRegs
    printfn ""
    printfn "Manually reading the program we see:"
    printfn """
Reg 0   Reg 1   Reg 2       Reg 3   Reg 4   Reg 5
=====   =====   =====       =====   =====   =====
acc     cntr    big         ip              num
0       1       10551282    4   0           297

0   addi 3 16 3
1   seti 1 7 1
2   seti 1 7 5  5: num -> 1

                "repeat"           "break"
3   mulr 1 5 4  4: ctr * num       4: ctr * num
4   eqrr 4 2 4  4: 0               4: 1 (= big)
5   addr 4 3 3  3: jump 0          3: jump 1
6   addi 3 1 3  3: jump 1             ------
7   addr 1 0 0     ------          0: acc -> acc + fct
8   addi 5 1 5  0: num++           5: num -> num + 1
9   gtrr 5 2 4  4: 0 (if num>big)  4: 1
10  addr 3 4 3  3: jump 0          3:jump 1
11  seti 2 2 3  3: goto 2 (3)        ------

                "repeat"            "break"
12  addi 1 1 1  1: 2 (inc ctr)      1: 10551283 (inc ctr)
13  gtrr 1 2 4  4: 0 (ctr > big)    4: 1 (ctr > big)
14  addr 4 3 3  3: jump 0           3: jump 1 (16)
15  seti 1 5 3  3: goto 1 (2)          ------

16  mulr 3 3 3  3: 64   out of bounds

17  addi 2 2 2
19  mulr 2 2 2
20  mulr 3 2 2
"""
    printfn "With enough coffee the intent of the program appears to"
    printfn "be to find the sum of the factors of %i ..." reg2
    printfn ""
        
    [1..reg2]
    |> List.filter (fun i -> reg2 % i = 0)
    |> List.sum
