// Learn more about F# at http://fsharp.org

open System
open Common

let readInput() =
    let lines = readLines "input.txt"
    [for line in lines do
        yield Seq.toArray line ] |> List.toArray


[<EntryPoint>]
let main argv =
    let input = readInput()
    0 // return an integer exit code
