// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions
open Common

let parseInput (lines:seq<string>) =
    let rec parseNextLine linesLeft result =
        match linesLeft with
        | head::tail -> match head with
                        | ParseRegex "Step (\w) must be finished before step (\w) can begin" [former; latter] -> parseNextLine tail ((former |> Seq.head, latter |> Seq.head)::result)
        | [] -> List.rev result

    parseNextLine (lines |> List.ofSeq) []

(*
let addAdjacentNode adjList node =
    let rec goThrough remaining result =
        match remaining with
        | first::second::rest ->  if first <= node && second >= node
                                    then (second::node::first::result |> List.rev )@rest
                                    else goThrough (second::rest) (first::result)
        | [sole] -> if sole < node then [sole; node] else [node; sole]
        | [] -> List.rev result

    if adjList = []
    then [ node ]
    else goThrough adjList []

(*
let buildGraph (input:(char*char) list) =
    let nodes = [for node in 'A'..'Z' do yield node, [] ] |> Map.ofList

    let rec addNodes (remainingEdges:(char*char) list) (result:Map<char, char list>) =
        match remainingEdges with
        | current::rest -> let former, latter = current
                           let adjList = result.[former]
                           let newList = addAdjacentNode adjList latter
                           addNodes rest (result.Add(former, newList))
        | [] -> result

    addNodes input nodes *)

let buildGraph (input:(char*char) list) =
    let nodes = [for node in 'A'..'Z' do yield node, [] ] |> Map.ofList

    let rec addNodes (remainingEdges:(char*char) list) (result:Map<char, char list>) =
        match remainingEdges with
        | current::rest -> let former, latter = current
                           let adjList = result.[former]
                           let newList =  latter::adjList
                           addNodes rest (result.Add(former, newList))
        | [] -> result

    addNodes input nodes

let isReady (graph:Map<char, char list>) visited node =
    let nodesPointing = set [for KeyValue(k, vals) in graph do
                             for v in vals do
                             if v = node then yield k]

    Set.isSubset nodesPointing (visited |> Set.ofList) || nodesPointing.Count = 0

let findFirst (graph:Map<char, char list>) =
    let allWithChildren = graph |> Map.toList |> List.map snd |> List.collect id |> Set.ofList
    let x = [for n in 'A'..'Z' do
                if not (Set.contains n allWithChildren) then
                    yield n
                ]
    x |> List.head


let phase1 graph start =
    let rec walk current (graph:Map<char, char list>) available result =
        let children = graph.[current]
        let allAvailable = graph.[current]@available
        let validAvailable = allAvailable |> List.filter (isReady graph (current::result))  |> List.sort
        match validAvailable with
        | next::_ -> let restAvailable = allAvailable |> Set.ofList |> Set.remove next |> Set.toList
                     walk next graph restAvailable  (current::result)
        | [] -> current::result |> List.rev

    walk start graph [] []
*)




[<EntryPoint>]
let main argv =
    let lines = readLines "input.txt"
    let input = parseInput lines
    0 // return an integer exit code
