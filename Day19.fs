(* a.cbf.pub/tx/___________________________________________/data.html *)

module Day19

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
let print obj = (printfn "%O" obj); obj

(* ================ Part A ================ *) 

open Day16

//type Registers = Reg of int[]
type Instruction = Inst of string * int * int * int
//type Operation = Registers -> int -> int -> int -> Registers
type IP = IP of int
type IPReg = IPReg of int
type State = State of (IP * IPReg * Registers)

let parseLine line : Instruction = 
    line
    |> rxMatch "([a-z]+) (\d+) (\d+) (\d+)" 
    |> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        Inst (grp 1, grpi 2, grpi 3, grpi 4)

let register size = Array.zeroCreate size |> Reg

let outOfBounds (program:Instruction[]) (IP ip:IP) =
    ip < 0 || ip >= program.Length

let setIpReg (state : State) =
    let (State ((IP ip), (IPReg ipReg), reg)) = state
    set reg ipReg ip |> ignore
    state

let readIpRegAndIncrement (state : State) =
    let (State ((IP ip), (IPReg ipReg), reg)) = state
    let newIP = IP (1 + get reg ipReg)
    State (newIP, IPReg ipReg, reg)

let apply reg (Inst (opName, A, B, C)) = 
    let after = opNames.[opName] reg A B C
    after

let tick (program : Instruction []) (state:State) : State option =
    let (State ((IP ip), (IPReg ipReg), reg)) = state
    if outOfBounds program (IP ip) then None else
    
    setIpReg state |> ignore
    apply reg program.[ip] |> ignore
    let newState = readIpRegAndIncrement state
    Some newState
    
let Part1 (input : string) =  // "result1" (*
    let program = input |> toLines |> Array.ofList |> Array.map parseLine
    
    let initial = State (IP 0, IPReg 3, register 6)
    
    initial 
    |> Seq.unfold (fun state ->
        let newState = tick program state
        match newState with
        | None -> None
        | Some newState -> Some (newState, newState))
    // List.ofSeq
    |> Seq.last
    |> fun (State (_, _, Reg registers)) -> registers.[0]
    

//*)

    
     
(* ================ Part B ================ *)

let Part2 result1 (input : string) =  "result2" (*
    input |> toLines |> Seq.map parseLine



//*)