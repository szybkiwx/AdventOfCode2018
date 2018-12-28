open System
open System

open Common
type State =
| Open
| Trees
| Lumberyard

let getAdjacentFields arr x0 y0 =
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr
    let fromX = max 0 (x0 - 1)
    let fromY = max 0 (y0 - 1)
    let toX = min (x0 + 1) (maxX - 1)
    let toY = min (y0 + 1) (maxY - 1)

    [for x in fromX..toX do
     for y in fromY..toY do
        if (x, y) <> (x0, y0) then yield arr.[x, y]]


let stateTransition (arr:State[,]) x y  =
    let oldState = arr.[x, y]
    let adjacentFields = getAdjacentFields arr x y
    let enoughAdjacentTrees = adjacentFields |> List.filter (fun f -> f = Trees) |> List.length >= 3
    let enoughAdjacentLumberyards = adjacentFields |> List.filter (fun f -> f = Lumberyard) |> List.length >= 3
    let lumberyardConditionsSatisfied = fun fields -> let g = fields
                                                                |> List.groupBy id
                                                                |> List.map (fun (s, sl) -> s, sl.Length)
                                                                |> Map.ofList
                                                      g.ContainsKey Trees && g.[Trees] >= 1 && g.ContainsKey Lumberyard && g.[Lumberyard] >= 1
    match oldState with
    | Open ->  if enoughAdjacentTrees then Trees else Open
    | Trees -> if enoughAdjacentLumberyards then Lumberyard else Trees
    | Lumberyard -> if lumberyardConditionsSatisfied adjacentFields then Lumberyard else Open


let readInput() =
    let input = readLines "input.txt" |> Seq.toList
    let xLenght = input.Length
    let yLenght = input |> Seq.head |> Seq.toList |> List.length


    let arr = Array2D.create xLenght yLenght State.Open

    [ for i in 0..input.Length - 1 do
      let line = input.[i] |> Seq.toList
      for j in 0..input.[i].Length - 1 do
          let state = match line.[j] with
                      | '.' -> Open
                      | '|' -> Trees
                      | '#' -> Lumberyard
          Array2D.set arr i j state
     ]|> ignore

    arr


let minutePass (arr:State[,]) =
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr
    let result = Array2D.copy arr

    [for x in 0..maxX - 1 do
     for y in 0..maxY - 1 do
        let newState = stateTransition arr x y
        Array2D.set result x y newState ]
    |> ignore

    result

let countResouces (arr:State[,])  =
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr
    let g = [for x in 0..maxX - 1 do
             for y in 0..maxY - 1 do
                yield arr.[x,y]]
            |> List.filter(fun x -> x = Trees || x = Lumberyard)
            |> List.groupBy id
            |> List.map(fun (s, sl) -> s, sl.Length)
            |> Map.ofList

    g.[Trees] * g.[Lumberyard]

let printField (arr:State[,]) minute =
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr

    printfn "Minute %d" minute

    [for x in 0..maxX - 1 do
     yield '\n'
     for y in 0..maxY - 1 do
        yield match arr.[x, y] with
              | Open -> '.'
              | Trees -> '|'
              | Lumberyard -> '#' ]
    |> List.iter(fun f ->  printf "%c" f)

    printf "\n\n ========================= \n\n"



let phase1 input maxMinutes =
    let rec loop acres cnt =
        if cnt = maxMinutes then acres
        else let newResult = minutePass acres
             //printField newResult (cnt + 1)
             loop newResult (cnt + 1)

    countResouces (loop input 0)



let areStatesEqual (arr1:State[,]) (arr2:State[,]) =
     let maxX = Array2D.length1 arr1
     let maxY = Array2D.length2 arr1

     [for x in 0..maxX - 1 do
      for y in 0..maxY - 1 do
        yield arr1.[x, y] = arr2.[x, y] ]
     |> List.forall id

let findInHistory (history:State[,] list) (arr:State[,]) =
    let rec loop remaining cnt =
        match remaining with
        | current::rest -> if areStatesEqual current arr
                                then true, history.Length - cnt - 1
                           else loop rest (cnt + 1)
        | [] -> false, -1

    loop history 0



let phase2 input minutes =
    let rec loop acres cnt history =
        let newResult = minutePass acres
        let found, foundIndex = findInHistory history newResult
        if found then
           cnt, foundIndex, newResult
        else
           loop newResult (cnt + 1) (newResult::history)


    let currentIndex, starIndex, start =  loop input 0 []
    let period = currentIndex - starIndex
    let remaining = (minutes-currentIndex) % period - 1
    phase1 start remaining

[<EntryPoint>]
let main argv =
    let input = readInput()
    let result1 = phase1 input 10
    let result2 = phase2 input 1000000000
    printfn "Hello World from F#!"
    0 // return an integer exit code
