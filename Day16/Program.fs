// Learn more about F# at http://fsharp.org
open Common
open System
open System.Reflection

type Input = {
    Op: int;
    Arg1: int;
    Arg2:int;
    Result: int;
}

type Sample = {
    Before: int[]
    Input: Input
    After: int[]
}

let readInput() =
    let rawlines = readLines "input.txt" |> List.ofSeq

    let rec loop lines last index =
        match lines with
        | current::rest -> if current = last then index else loop rest current (index + 1)

    let idx = loop rawlines "" 0
    let arr = rawlines |> Array.ofList
    let description = arr.[0..idx - 1]
    let parsedDescriptions =
        [ for i in 0.. 4 ..(description.Length - 1) do
             let before = match description.[i] with
                          | ParseRegex "Before: \[(\d), (\d), (\d), (\d)\]" [Int rA; Int rB; Int rC; Int rD] ->
                            [|rA; rB; rC; rD|]
             let input =  match description.[i+1] with
                          | ParseRegex "(1?\d) (\d) (\d) (\d)" [Int op; Int arg1; Int arg2; Int result] ->
                            {Op= op; Arg1=arg1; Arg2=arg2; Result=result}
             let after =  match description.[i+2] with
                          | ParseRegex "After:  \[(\d), (\d), (\d), (\d)\]" [Int rA; Int rB; Int rC; Int rD] ->
                            [|rA; rB; rC; rD|]

             yield {Before=before; After=after; Input = input}]

    let input = arr.[idx + 2..] |> List.ofArray |> List.map(fun x ->  match x with
                                                                             | ParseRegex "(\d) (\d) (\d) (\d)" [Int op; Int arg1; Int arg2; Int result] ->
                                                                               {Op= op; Arg1=arg1; Arg2=arg2; Result=result})
    parsedDescriptions, input


let addr (A:int) (B:int) (registers:int[]) =
    registers.[A] + registers.[B]

let addi (A:int) (B:int) (registers:int[]) =
    registers.[A] + B

let mulr (A:int) (B:int) (registers:int[]) =
    registers.[A] * registers.[B]

let muli (A:int) (B:int) (registers:int[]) =
    registers.[A] * B

let banr (A:int) (B:int) (registers:int[]) =
    registers.[A] &&& registers.[B]

let bani (A:int) (B:int) (registers:int[]) =
    registers.[A] &&& B

let borr (A:int) (B:int) (registers:int[]) =
    registers.[A] ||| registers.[B]

let bori (A:int) (B:int) (registers:int[]) =
    registers.[A] ||| B

let setr (A:int) (B:int) (registers:int[]) =
    registers.[A]

let seti (A:int) (B:int) (registers:int[]) =
    A

let gtir (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
    if A > registers.[B] then 1 else 0

let gtri (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
    if registers.[A] > B then 1 else 0

let gtrr (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
    if registers.[A] > registers.[B] then 1 else 0

let eqir (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
    if A = registers.[B] then 1 else 0

let eqri (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
    if registers.[A] = B  then 1 else 0

let eqrr (A:int) (B:int) (registers:int[]) =
//sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
    if registers.[A] = registers.[B] then 1 else 0

let getInstructions() =
    [addi;addr;muli;mulr;banr;bani;borr;bori;setr;seti;gtir;gtri;gtrr;eqri;eqir;eqrr]



let findMatch (sample:Sample) =

    let rec loop instructions matches =
        match instructions with
        | current::rest ->  let value = current sample.Input.Arg1 sample.Input.Arg2 sample.Before
                            let result = Array.copy sample.Before
                            Array.set result sample.Input.Result value
                            loop rest ((result = sample.After)::matches)
        | [] -> matches
    loop (getInstructions()) []


let phase1 (input:Sample list*Input list) =
    let samples, instructions = input

    [for sample in samples do
                yield findMatch sample ]
    |> List.map (fun lst -> lst
                            |> List.filter id
                            |> List.length)
    |> List.filter (fun x -> x >=3)
    |> List.length


let getInstructionMap() =
    [("addi", addi);("addr", addr);("muli", muli);("mulr", mulr); ("banr", banr);("bani", bani);("borr", borr);("bori", bori);("setr", setr);("seti", seti);("gtir", gtir);("gtri", gtri);("gtrr", gtrr);("eqri", eqri);("eqir", eqir);("eqrr", eqrr)] |> Map.ofList

let printReg (arr1:int[]) (arr2:int[]) =
    printfn "[%d, %d, %d, %d] -> [%d, %d, %d, %d]" arr1.[0] arr1.[1] arr1.[2] arr1.[3] arr2.[0] arr2.[1] arr2.[2] arr2.[3]

let findMatch2 (sample:Sample) =
    let instructionMap = getInstructionMap()
    let rec loop instructions matches =
        match instructions with
        | current::rest ->  let operation = instructionMap.[current]
                            let value = operation sample.Input.Arg1 sample.Input.Arg2 sample.Before
                            let result = Array.copy sample.Before

                            Array.set result sample.Input.Result value

                            if result = sample.After then
                                loop rest (current::matches)
                            else
                                loop rest matches
        | [] -> matches
    let names = instructionMap |> Map.toSeq |> Seq.map fst |> Seq.toList
    loop names []

let reduce (instructionsByOps:(int*Set<string>) list) =
    let rec loop input (instMap:Map<int, string>) =
        match input with
        | (currentOp, (opset:Set<string>))::rest ->
                let instruction = opset |> Set.toList |> List.head
                let newinstMap = instMap.Add(currentOp, instruction)
                let newRest = [ for (op, funcSet) in rest do yield op, funcSet.Remove instruction]
                              |> List.sortBy (fun (op, opset) -> opset.Count)
                loop newRest newinstMap
        | [] -> instMap

    loop (instructionsByOps|> List.sortBy (fun (op, opset) -> opset.Count) ) Map.empty


let execute (instructionMap:Map<int, int -> int -> int[] -> int>) (input:Input list) =
    let registers = [|0;0;0;0|]

    [for current in input do
       let operation = instructionMap.[current.Op]
       registers.[current.Result] <- operation current.Arg1 current.Arg2 registers ] |> ignore

    registers


let phase2 (input:Sample list*Input list) =
    let samples, instructions = input

    let opcodeMatches = [for sample in samples do yield sample.Input.Op, findMatch2 sample ]
    let byOp = opcodeMatches  |> List.map (fun (op,funList) -> op, funList |> Set.ofList)

    let byOpSets = byOp
                   |> List.groupBy (fun (op,funList) -> op)
                   |> List.map (fun (op, opFunList) -> op, opFunList |> List.map snd)
                   |> List.map (fun (op, funSetList) -> op, funSetList  |> Set.intersectMany)


    for (op, funcs) in byOpSets |> List.sortBy (fun (x, y) -> y.Count) do
        printf "%d -> " op
        printfn "%s"  (funcs |> Set.toList |> String.concat ",")

    let nameToFuncMap = getInstructionMap()
    let instructionMap = reduce byOpSets |> Map.toList

    execute (instructionMap |>  List.map(fun (op, name) -> op, nameToFuncMap.[name]) |> Map.ofList) instructions


[<EntryPoint>]
let main argv =
    let input = readInput()
    //let result = phase1 input

    let result2 = phase2 input
    0 // return an integer exit code
