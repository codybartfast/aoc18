(* a.cbf.pub/tx/z_Dcro40D65yOo6K1x8L5APhKnb2ggkkzhlvMtFnXyU/data.html *)

module Day13

#nowarn "0025"

open System

let toLines (text:string) = 
    text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofSeq
let toChars (str : string) = str.ToCharArray()

(* ================ Part A ================ *) 

type Turn = Straight | Left | Right
type Track = char[,]
type Cart = ((int * int) * (char * Turn))

let parseInput input =
    let inputGrid = 
        let nested = input |> toLines |> Array.ofList |> Array.map toChars
        let X = nested.[0].Length
        let Y = nested.Length
        Array2D.init X Y (fun x y -> nested.[y].[x])
    let track = 
        inputGrid |> Array2D.map (function
        | '<' | '>' -> '-'
        | 'v' | '^' -> '|'
        | chr -> chr)
    let carts = 
        let X = Array2D.length1 inputGrid
        let Y = Array2D.length2 inputGrid
        seq{for x in 0..X-1 do for y in 0..Y-1 do yield (x,y)}
        |> Seq.map (fun (x,y) -> ((x,y), inputGrid.[x,y]))
        |> Seq.choose (fun (coord, char) ->
            match char with
            | '<' | '>' | '^' | 'v' -> Some (coord, (char, Left))
            | _ -> None)
        |> List.ofSeq
    (track, carts)

let rec moveCart (track : Track) cart =
    let ((x,y),(dir,turn)) = cart
    let rail = track.[x,y]
    let rec newCart rail dir turn=
        match rail, dir, turn with
        | '-', '>', _ -> (x+1, y), (dir, turn)
        | '-', '<', _ -> (x-1, y), (dir, turn)
        | '|', '^', _ -> (x, y-1), (dir, turn)
        | '|', 'v', _ -> (x, y+1), (dir, turn)

        | '/', '>', _ -> newCart '|' '^' turn
        | '/', '<', _ -> newCart '|' 'v' turn
        | '/', '^', _ -> newCart '-' '>' turn
        | '/', 'v', _ -> newCart '-' '<' turn

        | '\\', '>', _ -> newCart '|' 'v' turn
        | '\\', '<', _ -> newCart '|' '^' turn
        | '\\', '^', _ -> newCart '-' '<' turn
        | '\\', 'v', _ -> newCart '-' '>' turn

        | '+', '>', Left -> newCart '|' '^' Straight
        | '+', '>', Straight -> newCart '-' '>' Right
        | '+', '>', Right -> newCart '|' 'v' Left

        | '+', '<', Left -> newCart '|' 'v' Straight
        | '+', '<', Straight -> newCart '-' '<' Right
        | '+', '<', Right -> newCart '|' '^' Left

        | '+', '^', Left -> newCart '-' '<' Straight
        | '+', '^', Straight -> newCart '|' '^' Right
        | '+', '^', Right -> newCart '-' '>' Left

        | '+', 'v', Left -> newCart '-' '>' Straight
        | '+', 'v', Straight -> newCart '|' 'v' Right
        | '+', 'v', Right -> newCart '-' '<' Left
        
        | _ -> failwith "Where to go? Where to go!"
    newCart rail dir turn

let moveCarts moveCart carts clearCrashes =        
    let colides carts cart =
        let (coord, _) = cart
        carts |> Seq.exists(fun (coord2, _) -> coord2 = coord)
    let remove coord =
        List.filter (fun cart -> (fst cart) <> coord)
    let rec moveCarts' moved toMove =
        match toMove with
        | [] -> moved
        | cart::tail ->
            let newCart = moveCart cart
            let collision = (colides moved newCart) || (colides tail newCart)
            match collision, clearCrashes with
            | true, false -> failwith (sprintf "crash! %O" (fst newCart))
            | true, true -> 
                let crashCoord = fst newCart
                moveCarts' (remove crashCoord moved) (remove crashCoord tail)
            | _ -> moveCarts' (newCart::moved) tail
    let carts = carts |> List.sortBy fst
    moveCarts' [] carts

let Part1 (input : string) =
    let (track, carts) = parseInput input
    let moveCart = moveCart track
    
    let rec chooChoo carts =
        try 
            chooChoo (moveCarts moveCart carts false)
        with | ex -> ex.Message
    chooChoo carts

(* ================ Part B ================ *) 

let Part2 result1 (input : string) = 
    let (track, carts) = parseInput input
    let moveCart = moveCart track
    
    let rec chooChoo carts =   
        match carts with
        | [survior] -> fst survior
        | _ -> chooChoo (moveCarts moveCart carts true)
    chooChoo carts
