open System
open System
open System
open System
open System

let movePointer (sequence:int list) current n =
    let length = sequence.Length
    (current + length - n) % length |> abs


let appendNewValue lst value =
    if value < 10 then
        value::lst
    else
        (value % 10)::1::lst


let phase1 runs =
    let start = 7::3::[]

    let rec loop lastIndex1 lastIndex2 (result:int list) cnt =
        let value1 = result.[lastIndex1]
        let value2 = result.[lastIndex2]
        let newValue = value1 + value2
        let newResult = appendNewValue result newValue

        let shift = newResult.Length - result.Length

        let next1 = movePointer newResult (lastIndex1 + shift) (1 + value1)
        let next2 = movePointer newResult (lastIndex2 + shift) (1 + value2)

        if cnt = runs + 10 then
            result
        else
          loop next1 next2 newResult (cnt + 1)

    loop 1 0 start 0 |> List.rev |> List.skip runs |> List.take 10 |> String.Concat

let appendNewValueToArray arr index value =
    if value < 10 then
        Array.set arr index value
        arr, 1
    else
        Array.set arr index 1
        Array.set arr (index + 1) (value % 10)
        arr, 2

let phase12 runs =
    let start = Array.zeroCreate (runs * 2)
    Array.set start 0 3
    Array.set start 1 7

    let rec loop lastIndex1 lastIndex2 cnt size =
        let value1 = start.[lastIndex1]
        let value2 = start.[lastIndex2]
        let newValue = value1 + value2
        let newResult, inc = appendNewValueToArray start size newValue
        let newSize = size + inc

        let next1 = (lastIndex1 +  1 + value1 ) % newSize
        let next2 = (lastIndex2 +  1 + value2 ) % newSize

        if cnt = runs + 11 then
            Array.skip runs start |> Array.take 10 |> String.Concat
        else
            loop next1 next2 (cnt + 1) newSize

    loop 0 1 0 2

(*let findSeq (recipes:int[]) size (score:int[]) =
    let toCompare1 = if size >= score.Length then recipes.[size-score.Length..size-1] else Array.zeroCreate score.Length
    let toCompare2 = if size > score.Length then recipes.[size-score.Length-1..size-2] else Array.zeroCreate score.Length

    let r1 = Array.zip toCompare1 score |> Array.fold (fun acc (x, y) -> acc && x = y) true
    let r2 = Array.zip toCompare2 score |> Array.fold (fun acc (x, y) -> acc && x = y) true
    r1 || r2
*)
let findSeq1 (recipes:int[]) size (score:int[]) =
    let toCompare1 = if size >= score.Length then recipes.[size-score.Length..size-1] else Array.zeroCreate score.Length
    Array.zip toCompare1 score |> Array.fold (fun acc (x, y) -> acc && x = y) true


let findSeq2 (recipes:int[]) size (score:int[]) =
    findSeq1 recipes (size - 1) score


(*
let findSeq (recipes:int[]) size (score:int[]) =

    let scoreSize = score.Length
    [for i in 0..size-1 do
        yield [for j in 0..scoreSize-1 do yield recipes.[j+i], score.[j] ] |> List.fold (fun acc (x, y) -> acc && x = y) true ]
    |> List.exists id *)



let phase2 (input:string) =
    let score = input |> Seq.toArray |> Array.map (fun x -> int x - int '0')

    let mutable start = Array.zeroCreate (30000000)
    Array.set start 0 3
    Array.set start 1 7

    let rec loop lastIndex1 lastIndex2 cnt size =
        let value1 = start.[lastIndex1]
        let value2 = start.[lastIndex2]
        let newValue = value1 + value2
        let newResult, inc = appendNewValueToArray start size newValue
        let newSize = size + inc

        let isFound = findSeq1 start newSize score
        let isFound2 = findSeq2 start newSize score
        if isFound  then
            newSize - score.Length
        elif isFound2 then
            newSize - score.Length - 1
        else
            if newSize + 2 > start.Length then
                start <- [|start;  Array.zeroCreate(newSize)|] |> Array.concat

            let next1 = (lastIndex1 +  1 + value1 ) % newSize
            let next2 = (lastIndex2 +  1 + value2 ) % newSize

            loop next1 next2 (cnt + 1) newSize

    loop 0 1 0 2

[<EntryPoint>]
let main argv =
    //let result1 = phase12 990941
    //let result2 = phase2 "59414"// "990941"
    let result2 = phase2 "990941"
    0 // return an integer exit code
