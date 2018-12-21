(* a.cbf.pub/tx/1gv9zFppmHeZUB0soHDbudNP4ASyHyho3rjJlZbMRkE/data.html *)

module Day21

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)

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
    let lines = input |> toLines
    let ipReg = lines.[0] |> rxMatch "\d" |> (fun m -> m.Value) |> int
    let program = lines |> Array.skip 1 |> Array.map parseLine
    (program, ipReg)

let run (program : Instruction[]) registers ipReg check =
    let toInt = (function true -> 1 | false -> 0)
    let rec tick (reg :int[]) ip =
        reg.[ipReg] <- ip 

        match check reg with 
        | Some resp -> resp 
        | _ ->

        let instr, A, B, C = program.[ip]
        match instr with
        | "addr" -> reg.[C] <- (reg.[A] + reg.[B]);
        | "addi" -> reg.[C] <- (reg.[A] + B);
        | "mulr" -> reg.[C] <- (reg.[A] * reg.[B]);
        | "muli" -> reg.[C] <- (reg.[A] * B);
        | "banr" -> reg.[C] <- (reg.[A] &&& reg.[B]);
        | "bani" -> reg.[C] <- (reg.[A] &&& B);
        | "borr" -> reg.[C] <- (reg.[A] ||| reg.[B]);
        | "bori" -> reg.[C] <- (reg.[A] ||| B);
        | "setr" -> reg.[C] <- (reg.[A]);
        | "seti" -> reg.[C] <- A;
        | "gtir" -> reg.[C] <- ((A > reg.[B]) |> toInt);
        | "gtri" -> reg.[C] <- ((reg.[A] > B) |> toInt);
        | "gtrr" -> reg.[C] <- ((reg.[A] > reg.[B]) |> toInt);
        | "eqir" -> reg.[C] <- ((A = reg.[B]) |> toInt);
        | "eqri" -> reg.[C] <- ((reg.[A] = B) |> toInt);
        | "eqrr" -> reg.[C] <- ((reg.[A] = reg.[B]) |> toInt);
        tick reg (reg.[ipReg] + 1)
    tick registers 0

let Part1 (input : string) =
    let (program, ipReg) = parse input 
    let reg = (Array.zeroCreate 6)

    let check (reg: int[]) =
        if reg.[ipReg] = 28 
        then Some reg.[4]
        else None

    run program reg ipReg check

(* ================ Part B ================ *)

type Inst28Candidates = {Last: int; Seen: Set<int>; GotAll: bool}

let fun6 (r4:int) (r5:int) candidates =
    let r5 = r4 ||| 65536   // 6
    let r4 = 1765573        // 7
    r4, r5, candidates

let fun28 (r4:int) (r5:int) candidates =
    let {Last= last; Seen= seen} = candidates
    let candidates =
        if seen.Contains r4         
            then {Last = last; Seen = seen; GotAll = true }
            else {Last = r4; Seen = seen.Add r4; GotAll = false }
    fun6 r4 r5 candidates

let rec fun8 (r4:int) (r5:int) candidates = 
    if candidates.GotAll = true then candidates.Last else
    let r1 = r5 &&& 255                 // 8
    let r4 = r4 + r1                    // 9
    let r4 = r4 &&& 16777215            // 10
    let r4 = r4 * 65899                 // 11
    let r4 = r4 &&& 16777215            // 12
    let r4, r5, candidates =        
        if 256 > r5 
        then fun28 r4 r5 candidates     // -> 28
        else r4, (r5/256), candidates   // 18-27
    fun8 r4 r5 candidates               // ->  8

let Part2 result1 (input : string) = 
    let (program, ipReg) = parse input 
    let reg = (Array.zeroCreate 6)

    let check (reg: int[]) =
        if reg.[ipReg] <> 8 then None else
        let candidtaes = {Last= 0; Seen= Set.empty; GotAll= false}
        Some (fun8 reg.[4] reg.[5] candidtaes)

    run program reg ipReg check
