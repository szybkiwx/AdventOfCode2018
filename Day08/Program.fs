// Learn more about F# at http://fsharp.org

open System
open System
open System
open System
open Common

let rec processChildren childrenCount (input:int[]) =
    let mutable pos = 0
    let mutable cnt = 0
    for i in 0..childrenCount-1 do

        let p, c = processNode (input.[pos..] |> List.ofArray)
        pos <- pos + p
        cnt <- cnt + c
    pos, cnt

and processNode (input: int list) =
    match input with
    |   childrenCount::metadataCount::rest -> let childEndPos, childEndSum = processChildren childrenCount ( rest |> Array.ofList )
                                              let endPos = childEndPos + metadataCount
                                              let restArray = rest |> Array.ofList
                                              let metadataSum = restArray.[childEndPos..endPos - 1] |> Array.sumBy id
                                              2 + endPos, childEndSum + metadataSum


let rec processChildren2 childrenCount (input:int[]) =
    let mutable pos = 0
    let sums = [for i in 0..childrenCount-1 do
                    let p, c = processNode2 (input.[pos..] |> List.ofArray)
                    pos <- pos + p
                    yield c]
    pos, sums

(*
and processNode2 (input: int list) =
    match input with
    |   childrenCount::metadataCount::rest ->
            let restArray = rest |> Array.ofList
            if childrenCount = 0 then
                let metadataSum = restArray.[0 .. metadataCount - 1] |> Array.sum
                2 + metadataCount, metadataSum
            else
                let childEndPos, sums = processChildren2 childrenCount ( rest |> Array.ofList )
                let endPos = childEndPos + metadataCount
                let metaIndexes = restArray.[childEndPos..endPos - 1]
                let filteredIndexes = metaIndexes |> Array.filter (fun x -> x > 0 && x <= sums.Length)
                let metaSum = filteredIndexes  |> Array.sumBy (fun x -> sums.[x - 1])
                endPos + metadataCount + 1, metaSum
*)
and processNode2 (input: int list) =
    match input with
    |   childrenCount::metadataCount::rest ->
            if childrenCount = 0 then
                let restArray = rest |> Array.ofList
                let metadataSum = restArray.[0 .. metadataCount - 1] |> Array.sum
                2 + metadataCount, metadataSum
            else
                let childEndPos, childSums = processChildren2 childrenCount ( rest |> Array.ofList )
                let endPos = childEndPos + metadataCount
                let restArray = rest |> Array.ofList
                let metadata = restArray.[childEndPos..endPos - 1]
                let childEndSum = metadata
                                        |> Array.map(fun x -> x - 1)
                                        |> Array.filter(fun x -> x >= 0 && x < childSums.Length)
                                        |> Array.map(fun x -> childSums.[x])
                                        |> Array.sum
                2 + endPos, childEndSum

let phase1 (input:int[]) =
    processNode (input |> Array.toList) |> snd

let phase2(input:int[]) =
    processNode2 (input |> Array.toList) |> snd

[<EntryPoint>]
let main argv =
    let data = readInput "input.txt" |> fun s -> s.Split() |> Array.map int

    //let result1 = phase1 [|2; 3; 0; 3; 10; 11; 12; 1; 1; 0; 1; 99; 2; 1; 1; 2|]
    //let result1 = phase1 data
    let result2 = phase2 [|2; 3; 0; 3; 10; 11; 12; 1; 1; 0; 1; 99; 2; 1; 1; 2|]
    let result22 = phase2  data
    printfn "Hello World from F#!"
    0 // return an integer exit code
