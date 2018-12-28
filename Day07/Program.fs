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




let phase1 (input:(char*char) list) =
    let firsts = input |> List.map fst |> Set.ofList
    let seconds =  input |> List.map snd |> Set.ofList
    let start = Set.difference  firsts seconds

    let rec loop (candidates:Set<char>) result =
        if Set.isEmpty candidates
        then result |> List.rev
        else
            let next = candidates |> Set.toList |> List.sort |> List.head
            let potentialCandidates = input
                                       |> List.filter (fun (x, y) -> x = next)
                                       |> List.map snd
                                       |> Set.ofList

            let resultSet = (next::result) |> Set.ofList
            let newCandidates = potentialCandidates
                                |> Set.filter (fun cand ->
                                    let predecessors = input |> List.filter(fun (x, y) -> y = cand)
                                    predecessors |> List.forall (fun (x, y) -> resultSet.Contains x))

            loop (Set.remove next (Set.union candidates newCandidates) ) (next::result)

    loop  start []


(*
let getKeysFromMap mmap = mmap |> Map.toSeq |> Seq.map fst |> Set.ofSeq


let updateWorkers (workedAt:Map<char, int>) noOfWokers =
    let rec loop remainingJobs worker result =
        match remainingJobs with
        | [] -> result
        | (k, v)::rest ->
            if v > 1 then
                if worker <= noOfWokers then
                    loop rest (worker + 1) ((k, v - 1)::result)
                else
                    loop rest worker ((k, v)::result)
            else loop rest (worker + 1) result

    let newWorked = (loop (workedAt |> Map.toList ) 1 []) |> Map.ofList
    newWorked, Set.difference (getKeysFromMap workedAt) (getKeysFromMap newWorked)


let chr2int chr =
    let value = int chr - int 'A' + 1
    value

let phase2 (input:(char*char) list)  timeBase workers =
    let firsts = input |> List.map fst |> Set.ofList
    let seconds =  input |> List.map snd |> Set.ofList
    let start = Set.difference  firsts seconds |> Set.toList |> List.map (fun chr -> chr, timeBase + chr2int chr ) |> Map.ofList

    let rec loop (workedAt:Map<char, int>) (result:Set<char>) cnt =
        if Map.isEmpty workedAt
        then cnt
        else
            let newWorkedAt, finished = updateWorkers workedAt workers

            if not (Set.isEmpty finished) then
                let newCandidates = [for next in finished do
                                        let potentialCandidates = input
                                                                   |> List.filter (fun (x, y) -> x = next)
                                                                   |> List.map snd
                                                                   |> Set.ofList

                                        let resultSet = result.Add next
                                        yield potentialCandidates  |> Set.filter (fun cand ->
                                                                                input |> List.filter(fun (x, y) -> y = cand)
                                                                                      |> List.forall (fun (x, y) -> resultSet.Contains x)
                                                                                      )
                                      ]
                                        |> Set.ofList
                                        |> Set.fold (fun acc x -> Set.union acc x) Set.empty
                                        |> Set.toList
                                        |> List.map (fun chr -> chr, timeBase + chr2int chr )
                                        |> Map.ofList

                loop (Map.fold (fun state k v -> state.Add(k, v) ) newCandidates newWorkedAt) (Set.union finished result) (cnt + 1)
            else
                loop newWorkedAt result (cnt + 1)
    loop start Set.empty 0

*)



type Worker = {
    node:char
    ticks:int
    free:bool
}


let chr2int chr =
    let value = int chr - int 'A' + 1
    value

let getKeysFromMap mmap = mmap |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let distributeWork workers candidates timeBase =
    let rec loop remainingWorkers remainingCandidates updatedWorkers =
        match remainingWorkers with
        | worker::rest ->   match remainingCandidates with
                            | candidate::restCandidates ->
                                let newWorker = {worker with node=candidate;ticks=(timeBase + chr2int candidate);free=false}
                                loop rest restCandidates (newWorker::updatedWorkers)
                            | [] -> loop rest [] (worker::updatedWorkers)
        | [] -> updatedWorkers@(workers |> List.filter (fun x -> not x.free)) , remainingCandidates

    loop (workers |> List.filter (fun x -> x.free)) candidates []

let updateWorkers workers =
    let rec loop remainingWorkers updatedWorkers (finished:Set<char>) =
         match remainingWorkers with
         | worker::rest ->  if worker.ticks = 1 then
                                let newWorker = {node='-'; free=true; ticks= -1; }
                                loop rest (newWorker::updatedWorkers) (finished.Add worker.node)
                            else
                                let newWorker = {worker with ticks=worker.ticks - 1 }
                                loop rest (newWorker::updatedWorkers) finished
         | [] -> updatedWorkers, finished

    loop workers [] Set.empty

let allWorkersFinished (workers:Worker list) =
    workers|> List.forall (fun x -> x.node <> '-' && x.free)

let phase2 chars (input:(char*char) list) baseTime workers =
    let firsts = input |> List.map fst |> Set.ofList
    let seconds =  input |> List.map snd |> Set.ofList
    let start = Set.difference  firsts seconds

    let workers = [for i in 1..workers do yield  {node = '-'; ticks = -1; free=true}]

    let rec loop (candidates:Set<char>) (workers:Worker list) (result:Set<char>) cnt =
        if chars = result
        then cnt
        else
            let candidatesToProcess = candidates |> Set.toList |> List.sort
            let workersWithNewJobs, remainingCandidates = distributeWork workers candidatesToProcess baseTime
            let updatedWorkers, finished = updateWorkers workersWithNewJobs
            if finished.Count > 0 then
                printfn "ss"
            let potentialCandidates = input
                                       |> List.filter (fun (x, y) -> finished.Contains x)
                                       |> List.map snd
                                       |> Set.ofList

            let resultSet = Set.union finished result
            let newCandidates = potentialCandidates
                                |> Set.filter (fun cand ->
                                    let predecessors = input |> List.filter(fun (x, y) -> y = cand)
                                    predecessors |> List.forall (fun (x, y) -> resultSet.Contains x))
            let setCandidates = Set.union (remainingCandidates|> Set.ofList) newCandidates


            loop (Set.difference setCandidates finished) updatedWorkers resultSet (cnt + 1)

    loop  start workers Set.empty 0

[<EntryPoint>]
let main argv =
    let lines = readLines "input.txt"
    let input = parseInput lines
    let result = phase1 input |> Set.ofList
    let result2 = phase2 result input 60 5
    0 // return an integer exit code
