(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day16

// #nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
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

type Registers = Reg of int[]
type Instruction = Inst of int * int * int * int
type Operation = Registers -> int -> int -> int -> Registers
type Sample = Sample of Registers*Instruction*Registers

let splitInput (input:string) =
    input |> rxSplit @"(\r\n){4,4}"
    
let parsePart1 input = 
    input
    |> rxSplit @"(\r\n){2,2}"
    |> Array.map (fun entry -> 
        rxMatches @"(-?\d+)" entry
        |> Seq.map(fun m -> m.Value |> int))
    |> Array.map (fun seq -> 
        seq 
        |> Array.ofSeq 
        |> (fun array -> (fun i -> array.[i]))
        |> (fun f -> 
            (Sample (Reg [|f 0; f 1; f 2; f 3|], 
                    Inst (f 4, f 5, f 6, f 7),
                    Reg [|f 8; f 9; f 10; f 11|] ))))

//let create  = function
//    | None -> Reg [|0; 0; 0; 0|]
//    | Some []


let get (Reg registers) i = registers.[i]
let set (Reg registers) i v = registers.[i] <- v; Reg registers

let opNames : Map<string,Operation> = 
    let toInt = (function true -> 1 | false -> 0)
    Map [
        ("addr", (fun reg A B C -> set reg C (get reg A + get reg B)));
        ("addi", (fun reg A B C -> set reg C (get reg A + B)));
        ("mulr", (fun reg A B C -> set reg C (get reg A * get reg B)));
        ("muli", (fun reg A B C -> set reg C (get reg A * B)));
        ("banr", (fun reg A B C -> set reg C (get reg A &&& get reg B)));
        ("bani", (fun reg A B C -> set reg C (get reg A &&& B)));
        ("borr", (fun reg A B C -> set reg C (get reg A ||| get reg B)));
        ("bori", (fun reg A B C -> set reg C (get reg A ||| B)));
        ("setr", (fun reg A B C -> set reg C (get reg A)));
        ("seti", (fun reg A B C -> set reg C A));
        ("gtir", (fun reg A B C -> set reg C ((A > get reg B) |> toInt) ));
        ("gtri", (fun reg A B C -> set reg C ((get reg A > B) |> toInt) ));
        ("gtrr", (fun reg A B C -> set reg C ((get reg A > get reg B) |> toInt) ));
        ("eqir", (fun reg A B C -> set reg C ((A = get reg B) |> toInt) ));
        ("eqri", (fun reg A B C -> set reg C ((get reg A = B) |> toInt) ));
        ("eqrr", (fun reg A B C -> set reg C ((get reg A = get reg B) |> toInt) )); ]

let consistent (capture:Sample) (operation:Operation) =
    let (Sample ((Reg before), instr, after)) = capture
    let (Inst (_, A, B, C)) = instr
    
    let expected = operation (Array.copy before |> Reg) A B C
    expected = after

let findConsistent opNames capture  =
    opNames
    |> Map.toSeq
    |> Seq.filter(fun (_,op) -> consistent capture op)
    
let example () = 
    Sample ((Reg [|3; 2; 1; 1|]), Inst (9, 2, 1, 2), (Reg [|3; 2; 2; 1|]))
                
let Part1 (input : string) =  // "result1" (*
    (splitInput input).[0]
    |> parsePart1
    |> Seq.map (fun capture -> findConsistent opNames capture)
    |> Seq.map (Seq.length)
    |> Seq.filter ((<=) 3)
    |> Seq.length

//*)

    

(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)