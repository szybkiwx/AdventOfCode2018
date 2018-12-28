// Learn more about F# at http://fsharp.org
open Common
open System
open System
open System

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
    if A > registers.[B] then 1 else 0

let gtri (A:int) (B:int) (registers:int[]) =
    if registers.[A] > B then 1 else 0

let gtrr (A:int) (B:int) (registers:int[]) =
    if registers.[A] > registers.[B] then 1 else 0

let eqir (A:int) (B:int) (registers:int[]) =
    if A = registers.[B] then 1 else 0

let eqri (A:int) (B:int) (registers:int[]) =
    if registers.[A] = B  then 1 else 0

let eqrr (A:int) (B:int) (registers:int[]) =
    if registers.[A] = registers.[B] then 1 else 0

let getInstructionMap() =
    [("addi", addi);("addr", addr);("muli", muli);("mulr", mulr); ("banr", banr);("bani", bani);("borr", borr);("bori", bori);("setr", setr);("seti", seti);("gtir", gtir);("gtri", gtri);("gtrr", gtrr);("eqri", eqri);("eqir", eqir);("eqrr", eqrr)] |> Map.ofList


type Input = {
    OpCode: string;
    Op: (int -> int -> int[] -> int);
    Arg1: int;
    Arg2:int;
    Result: int;
}


let readInput() =
    let pointerStr::instructionStrings = readLines "input.txt" |> Seq.toList
    let pointer = match pointerStr with
                  | ParseRegex "#ip (\d)" [Int pointer] -> pointer

    let instMap = getInstructionMap()
    let instructions =
        [for current in instructionStrings do
            yield match current with
                  | ParseRegex "([a-z]{4}) (\d+) (\d+) (\d+)" [opcode; Int arg1; Int arg2; Int result ] ->
                     {OpCode=opcode;Op=instMap.[opcode]; Arg1=arg1;Arg2=arg2;Result=result}
        ] |> List.toArray
    pointer, instructions

let reg2str (reg:int[]) =
    reg |> Array.map (fun x -> sprintf "%d" x) |> String.concat ", "

let printDebug ip regBefore regAfter instr =
    printfn "ip=%d [%s] %s %d %d %d [%s]" ip (reg2str regBefore) instr.OpCode instr.Arg1 instr.Arg2 instr.Result (reg2str regAfter)

let phase1 (boundPtr:int) (instructions:Input[]) =
    (*let mutable ip = 0
    let mutable reg = [|0;0;0;0;0;0|]

    while ip < instructions.Length do
        let instr = instructions.[ip]
        reg.[boundPtr] <- ip
        reg.[instr.Result] <- instr.Op instr.Arg1 instr.Arg2 reg
        ip<-(reg.[boundPtr] + 1)
    reg*)
    let rec loop ip (registers:int[]) =
        if ip >= instructions.Length  then
            registers
        else
            let instr = instructions.[ip]
            Array.set registers boundPtr ip

            let result = instr.Op instr.Arg1 instr.Arg2 registers
            let newRegisters = Array.copy registers
            Array.set newRegisters instr.Result result
            loop (newRegisters.[boundPtr] + 1) newRegisters

    loop 0 [|0;0;0;0;0;0|]


let phase2() =
   (*
    let mutable value = 0
    let start = 11303916
    for i in 1..start do
        for j in 1..start do
            let x = i * j
            if x = start then
                value <-value + i

    value
*)
    let start = 10551276
    [for i in 1..start do
        if start % i = 0 then yield i]
    |> List.sum

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let pointer, instructions = readInput()
    //let result = phase1 pointer instructions
    let result2 = phase2 ()
    0 // return an integer exit code
