open System
open System
open System.Numerics
open Common
open System.IO

let debug pots = pots |> Array.map(fun x -> if x = 1 then '#' else '.') |> String.Concat

let parseState str =
    str
    |> Seq.map(fun x -> if x = '#' then 1 else 0)
    |> Seq.toArray


let parseGeneration str =
    match str with
    | ParseRegex "([#.]{5}) => ([#.])" [stateStr; newStateStr] ->  parseState stateStr, if newStateStr = "#" then 1 else 0

let readInput =
    let input = readLines "input.txt"
    input |> Seq.map parseGeneration |> Seq.toList

let cmp (arr1:int[]) (arr2:int[]) =
    [for i in 0 .. arr1.Length - 1 do
        yield arr1.[i] = arr2.[i] ] |> List.forall id

let findState (input:(int[]*int) list) targetState =
    [for (state,outcome) in input do
        if cmp state targetState then
            yield outcome
    ] |> List.tryHead

let sumUpPots (pots:int[]) =
    [for i in 0.. pots.Length - 5 - 1 do
         if pots.[i] = 1 then
            yield i - 5
     ] |> List.sum


let phase1 initialState input =

    let initPots = [|[| 0;0;0;0;0 |]; initialState ; [| 0;0;0;0;0 |] |] |> Array.concat

    let rec loop (pots:int[]) cnt =
        printfn "%s" (debug pots)
        if cnt = 20 then
           sumUpPots pots
        else
            let len = pots.Length
            let newPots = Array.create len 0
            for i in 0.. (len - 5 - 1) do
                let tried = pots.[i..(i + 5 - 1)]
                let outcomeOption = findState input tried
                let option = match outcomeOption with
                             | Some x -> x
                             | None -> 0
                Array.set newPots (i + 2) option
            let nextPots = [|newPots; [|0|]|] |> Array.concat
            loop nextPots (cnt + 1)

    loop initPots 0

let cmp2 (arr1:int[]) (arr2:int[]) =
    let nArr2 = if arr1.Length > arr2.Length then [|arr2; Array.create (arr1.Length - arr2.Length) 0|] |> Array.concat else arr2
    let nArr1 = if arr1.Length < arr2.Length then [|arr1; Array.create (arr2.Length - arr1.Length) 0|] |> Array.concat else arr1
    [for i in 0..nArr1.Length - 1 do yield nArr1.[i] = nArr2.[i] ] |> List.forall id

let findInHistory (history:int[] list) (pots:int[]) =
    let rec loop entries i =
        match entries with
        | entry::rest -> if cmp2 entry pots then
                            i
                         else
                            loop rest (i + 1)
        | [] -> -1

    loop (history |> List.rev) 0

let phase2 initialState input =
    let initPots = [|[| 0;0;0;0;0 |]; initialState ; [| 0;0;0;0;0 |] |] |> Array.concat
    let rec loop (pots:int[]) cnt sums (lines:string list) =
        let len = pots.Length
        let newPots = Array.create len 0
        for i in 0.. (len - 5 - 1) do
            let tried = pots.[i..(i + 5 - 1)]
            let outcomeOption = findState input tried
            let option = match outcomeOption with
                         | Some x -> x
                         | None -> 0
            Array.set newPots (i + 2) option
        let nextPots = [|newPots; [|0|]|] |> Array.concat
        if cnt = 157 then //magic number found by ibserving the output, from this point pattern started to repeat
                          //and each step was the same as previous shifted one place to the right
            sumUpPots nextPots
        else
            let line = sprintf "%d: %s" cnt (debug nextPots)
            loop nextPots (cnt + 1) sums (line::lines)


    //let lines = loop initPots 0  []
    //File.WriteAllLines (@".\output.txt", lines |> List.rev) |> ignore

    let sum = loop initPots 0 [] []
    let iterations = BigInteger.Parse("50000000000") - bigint 158
    bigint sum + iterations * bigint 42 //the righ shift for each step resulted in sum bigger by 42

[<EntryPoint>]
let main argv =
    //let initial = "###..###....####.###...#..#...##...#..#....#.##.##.#..#.#..##.#####..######....#....##..#...#...#.#"
    let initial = "#.##.#.##..#.#...##...#......##..#..###..##..#.#.....##..###...#.#..#...######...#####..##....#..###"
    let initialState = initial |> parseState
    let input = readInput

    //let result1 = phase1 initialState input
    let result2 = phase2 initialState input


    0 // return an integer exit code
