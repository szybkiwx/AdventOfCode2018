open System
open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type Claim = {
    Id: int;
    Top: int;
    Left: int;
    Width: int;
    Height: int;
}

let (|Int|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


let parseClaim str =
   match str with
   |    ParseRegex "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [Int id; Int left; Int top; Int width; Int height]
            ->  {Id = id; Top = top; Left = left; Width= width; Height= height}


let addClaim (claim:Claim) (placed:Set<int*int>) (intersectioned:Set<int*int>) =
    let fabricPoints = [for x in claim.Left..(claim.Left + claim.Width - 1) do
                        for y in claim.Top..(claim.Top + claim.Height - 1) do
                        yield x, y]

    let rec addClaimPoint (points:(int*int) list) (currplaced:Set<int*int>) (currintersectioned:Set<int*int>) =
        match points with
        |   current::rest -> if currplaced.Contains current && not (currintersectioned.Contains current)
                             then addClaimPoint rest currplaced (currintersectioned.Add current)
                             else addClaimPoint rest (currplaced.Add current) currintersectioned


        |   [] -> currplaced, currintersectioned
    addClaimPoint fabricPoints placed intersectioned

let day1 (input:seq<Claim>) =

    let rec placeClaims (claims:Claim list) (placed:Set<int*int>) (intersectioned:Set<int*int>) =
        match claims with
        |   current::rest -> let newPlaced, newIntersectioned = addClaim current placed intersectioned
                             placeClaims rest newPlaced newIntersectioned
        |   [] -> intersectioned.Count

    placeClaims (input |> List.ofSeq) Set.empty Set.empty


let doOverlap (c1: Claim) (c2: Claim) =
     c1.Left < c2.Left + c2.Width &&
     c1.Left + c1.Width > c2.Left &&
     c1.Top < c2.Top + c2.Height &&
     c1.Height + c1.Top > c2.Top


let day2 (input:seq<Claim>) =
    let cart = [for c1 in input do
                for c2 in input do
                        if c1 <> c2 then yield c1, c2]

    let rec findOverlaping pairs (overlapping:Set<Claim>) =
        match pairs with
        |   current::rest -> let c1, c2 = current
                             if doOverlap c1 c2
                             then findOverlaping rest (overlapping.Add(c1).Add(c2))
                             else findOverlaping rest overlapping
        |   [] -> overlapping

    let overlapping = findOverlaping cart Set.empty |>  Set.map (fun x -> x.Id)
    let original = input |> Set.ofSeq |> Set.map (fun x -> x.Id)
    Set.difference original overlapping



[<EntryPoint>]
let main argv =

    let input = readLines "input.txt" |> Seq.map parseClaim
    //let result1 = day1 input
    let result2 = day2 input
    //let claim = parseClaim "#1156 @ 755,816: 29x14"

    //printfn "%d" result1
    0 // return an integer exit code
