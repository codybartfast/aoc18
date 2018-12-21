(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day21

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    //|> List.ofArray
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
    let lines = input |> toLines
    let ipReg = lines.[0] |> rxMatch "\d" |> (fun m -> m.Value) |> int
    let program = lines |> Array.skip 1 |> Array.map parseLine
    (program, ipReg)

let run (program : Instruction[]) registers ipReg =
    let toInt = (function true -> 1 | false -> 0)
    let rec tick (reg :int[]) ip =
        reg.[ipReg] <- ip 
        (print reg)
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
    let reg = (Array.zeroCreate 6)
    reg.[0] <- 47965 
    run program reg ipReg 
    


//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)