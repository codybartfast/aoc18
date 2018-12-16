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

let consistent (sample:Sample) (operation:Operation) =
    let (Sample ((Reg before), instr, after)) = sample
    let (Inst (_, A, B, C)) = instr
    
    let expected = operation (Array.copy before |> Reg) A B C
    expected = after

let findConsistent opNames sample  =
    opNames
    |> Map.toSeq
    |> Seq.filter(fun (_,op) -> consistent sample op)
    
let example () = 
    Sample ((Reg [|3; 2; 1; 1|]), Inst (9, 2, 1, 2), (Reg [|3; 2; 2; 1|]))
                
let Part1 (input : string) =  // "result1" (*
    (splitInput input).[0]
    |> parsePart1
    |> Seq.map (fun sample -> findConsistent opNames sample)
    |> Seq.map (Seq.length)
    |> Seq.filter ((<=) 3)
    |> Seq.length

//*)
   

(* ================ Part B ================ *)
let parsePart2 input =
    input
    |> toLines
    |> List.map (rxSplit " " >> Array.map int)
    |> List.map (fun [|OP; A; B; C|] -> Inst (OP, A, B, C ))
    

let removeName potentialNames name =
    potentialNames
    |> List.map (fun (code, names) ->
        (code, Set.remove name names))
    |> List.filter (fun (_, names) -> not names.IsEmpty)

let potentialNames samples =
    samples
    |> List.ofSeq
    |> List.map( fun sample -> 
        let (Sample (_, instruction, _)) = sample
        let (Inst (code, _, _, _)) = instruction
        (code, findConsistent opNames sample |> Seq.map fst |> Set))
    |> List.groupBy fst
    |> List.map (fun (code, seqOpMatches) -> 
        let matchSets = seqOpMatches |> Seq.map snd
        let potentialNames = matchSets |> Seq.reduce Set.intersect
        (code, potentialNames))

let codeMap potentialNames = 
    let rec buildCodeMap (known:Map<int,string>) (potentialNames: (int*Set<string>) list) =
        if List.isEmpty potentialNames then known 
        else
        let singles = 
            potentialNames
            |> List.filter (fun (_, names) ->
                Set.count names = 1)
        if singles.IsEmpty then failwith "oops"
        let (code, name) = singles.Head |> fun (code, nameSet) -> (code, Seq.head nameSet)
        let newMap = Map.add code name known
        let potNames = removeName potentialNames name
        buildCodeMap newMap potNames
    buildCodeMap Map.empty potentialNames

let opCodes (codeMap : Map<int,string>) =
    (fun code -> 
        let name = codeMap.[code]
        opNames.[name])

let Part2 result1 (input : string) = 
    let [|input1; input2|] = splitInput input

    let samples = parsePart1 input1
    let potentialNames = potentialNames samples
    let codeMap = codeMap potentialNames
    let opCode = opCodes codeMap

    let instructions = parsePart2 input2
    let registry = Reg [|0; 0; 0; 0|]
    let apply reg (Inst (op, A, B, C)) = 
        (opCode op) reg A B C
    (registry, instructions)
    ||> List.fold apply
    |> fun (Reg values) -> values.[0]