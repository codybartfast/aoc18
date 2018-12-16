(* a.cbf.pub/tx/12RGCf4t9gNRTjGM8GSOw7x-fr7xZpjAso6unGwe460/data.html *)

module Day16

#nowarn "0025"

open System
open System.Text.RegularExpressions

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = 
    Regex.Split(str, pattern) 
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> List.ofArray

(* ================ Part A ================ *) 

type Registers = Reg of int[]
type Instruction = Inst of int * int * int * int
type Operation = Registers -> int -> int -> int -> Registers
type Sample = Sample of Registers*Instruction*Registers

let splitInput = rxSplit @"(\r\n){4,4}"
    
let parsePart1 = 
    rxSplit @"(\r\n){2,2}"
    >> List.map (
        rxMatches @"(-?\d+)"
        >> Seq.map(fun m -> m.Value |> int))
    >> List.map (
        List.ofSeq 
        >> (fun array -> (fun i -> array.[i]))
        >> (fun f -> 
            (Sample (Reg [|f 0; f 1; f 2; f 3|], 
                    Inst (f 4, f 5, f 6, f 7),
                    Reg [|f 8; f 9; f 10; f 11|] ))))

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
        ("gtrr", (fun reg A B C -> 
                        set reg C ((get reg A > get reg B) |> toInt) ));
        ("eqir", (fun reg A B C -> set reg C ((A = get reg B) |> toInt) ));
        ("eqri", (fun reg A B C -> set reg C ((get reg A = B) |> toInt) ));
        ("eqrr", (fun reg A B C -> 
                        set reg C ((get reg A = get reg B) |> toInt) ));]

let consistent (sample:Sample) (operation:Operation) =
    let (Sample ((Reg before), instr, after)) = sample
    let (Inst (_, A, B, C)) = instr   
    let expected = operation (Array.copy before |> Reg) A B C
    expected = after

let findConsistent sample =
    Map.toSeq >> Seq.filter(fun (_,op) -> consistent sample op)
                   
let Part1 (input : string) =  
    (splitInput input).[0]
    |> parsePart1
    |> Seq.filter (fun sample -> 
        findConsistent sample opNames
        |> Seq.length
        |> ((<=) 3))
    |> Seq.length 

(* ================ Part B ================ *)

let parsePart2 =
    toLines
    >> List.map (rxSplit " " >> List.map int)
    >> List.map (fun [OP; A; B; C] -> Inst (OP, A, B, C ))

let potentialNames =
    List.map(fun sample -> 
        let (Sample (_, instruction, _)) = sample
        let (Inst (code, _, _, _)) = instruction
        (code, findConsistent sample opNames|> Seq.map fst |> Set))
    >> List.groupBy fst
    >> List.map (fun (code, seqOpMatches) -> 
        let potNameSet = 
            seqOpMatches 
            |> Seq.map snd 
            |> Seq.reduce Set.intersect
        (code, potNameSet))

let codeMap potentialNames = 
    let removeName name =
        List.map (fun (code, names) ->
            (code, Set.remove name names))
        >> List.filter (fun (_, names) -> not names.IsEmpty)
    let rec buildCodeMap known potentialNames =
        if List.isEmpty potentialNames then known 
        else
        let singles = 
            potentialNames
            |> List.filter (fun (_, names) ->
                Set.count names = 1)
        let (code, name) = 
            singles.Head 
            |> fun (code, nameSet) -> 
                (code, Seq.exactlyOne nameSet)
        buildCodeMap 
            (Map.add code name known) 
            (removeName name potentialNames)
    buildCodeMap Map.empty potentialNames

let opCodes (codeMap : Map<int,string>) =
    (fun code -> opNames.[codeMap.[code]])

let Part2 result1 (input : string) = 
    let [input1; input2] = splitInput input

    let samples = parsePart1 input1
    let potentialNames = potentialNames samples
    let codeMap = codeMap potentialNames

    let opCode : int -> Operation = opCodes codeMap
    let instructions = parsePart2 input2
    let registry = Reg [|0; 0; 0; 0|]
    let apply reg (Inst (op, A, B, C)) = 
        (opCode op) reg A B C

    (registry, instructions)
    ||> List.fold apply
    |> fun (Reg regValues) -> regValues.[0]
