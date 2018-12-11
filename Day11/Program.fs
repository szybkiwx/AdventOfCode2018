// Learn more about F# at http://fsharp.org

open System


let calcPowerLevel x y serialNumber =
    let rackId = x + 10
    let initPowerLevel = rackId * y
    let increasedPowerLevel = initPowerLevel + serialNumber
    let multipliedPowerLevel = increasedPowerLevel * rackId
    let hundretsDigit = (multipliedPowerLevel / 100 ) % 10
    hundretsDigit - 5

let createGrid serialNumber =
    [for x in 1..300 do
     for y in 1..300 do
        yield ( (x, y), calcPowerLevel x y serialNumber )
     ] |> Map.ofList

let sumUp (grid:Map<(int*int), int>) point size =
    let x, y = point
    [for i in 0..size - 1 do
     for j in 0..size - 1 do
         yield grid.[(x + i, y + j)]
    ] |> List.sum

let phase1 serialNumber =
    let grid = createGrid serialNumber
    [for x in 1..298 do
     for y in 1..298 do
        yield (x, y), sumUp grid (x, y) 3
    ] |> List.sortByDescending snd |> List.head |> fst



let phase2 serialNumber =
    let grid = createGrid serialNumber
    [for size in 1..21 do
     for x in 1..300 - size + 1 do
     for y in 1..300 - size + 1 do
        yield (x, y, size), sumUp grid (x, y) size
    ] |> List.sortByDescending snd |> List.head |> fst

(*let sumUp2 (grid:Map<(int*int), int>) (prevLayer:Map<(int*int), int>) point size  =
    let x, y = point
    [for i in 0..size - 1 do
        yield grid.[(x + i, y + size - 1)] + grid.[(x + size - 1, y + i)]
        ]
    |> List.sum
    |> fun value -> value + prevLayer.[(x, y)]


let phase2 serialNumber =
    let grid = createGrid serialNumber
    let rec calculateNextLayer size prevLayer =
        if size > 20 then
            prevLayer
        else
            let currentLayer = [ for x in 1..300 - size + 1 do
                                  for y in 1..300 - size + 1 do
                                    yield (x, y), sumUp2 grid prevLayer (x, y) size
                                ] |> Map.ofList
            calculateNextLayer (size + 1) currentLayer

    let initPrev = [for i in 1..300 do
                     for j in 1..300 do
                        yield (i, j), 0 ] |> Map.ofList

    calculateNextLayer 1 initPrev |> Map.toList |> List.sortByDescending snd |> List.head |> fst
    // |> List.sortByDescending snd |> List.head |> fst *)



[<EntryPoint>]
let main argv =

    //let result1 = phase1 1723
    let result2 = phase2 1723
    0 // return an integer exit code
