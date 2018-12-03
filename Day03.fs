(* a.cbf.pub/tx/03QNToJ2eijfMPrc1yPKZImYl0qU-BgvcYOuz2SNKao/data.html *)

module Day03

open System.Text.RegularExpressions

let toLines (text:string) = text.Split('\n') |> List.ofSeq 
let groupValue (m:Match) (i:int) = m.Groups.[i].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let filterCount predicate = Seq.filter predicate >> Seq.length

(* ================ Part A ================ *) 

let parseLine line =
    line
    |> rxMatch "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    |> fun m -> 
        let grp = groupValue m >> int
        grp 1, (grp 2, grp 3), (grp 4, grp 5)
    
let toEdges (claimId, (left, top), (width, height)) =
    claimId, (left, left + (width - 1)), (top, top + (height - 1)) 

let enumCoords (claimId, (left, right), (top, bottom)) =
    seq{for x in left..right do 
        for y in top..bottom do
        yield (claimId, (x,y))}

let Part1 input = 
    input |> toLines
    |> Seq.collect (parseLine >> toEdges >> enumCoords)
    |> Seq.countBy (fun (_, coord) -> coord)
    |> filterCount (fun (_, count) -> count > 1)    
      
(* ================ Part B ================ *)

let sidesOverlap (start1, end1) (start2, end2) =
    ((start1 >= start2 && start1 <= end2) 
    || (start2 >= start1 && start2 <= end1))

let claimsOverlap (_, horiz1, vert1) (_, horiz2, vert2) =
    sidesOverlap horiz1 horiz2  &&  sidesOverlap vert1 vert2

let overlapsOthersIn claims claim =
    claims
    |> Seq.exists (fun otherClaim ->
        (otherClaim <> claim) && claimsOverlap otherClaim claim)
 
let Part2 result1 input = 
    let claims = 
        input |> toLines
        |> List.map (parseLine >> toEdges)
    claims
    |> Seq.filter (overlapsOthersIn claims >> not)
    |> Seq.exactlyOne 
    |> fun (claimId, _, _) -> claimId
