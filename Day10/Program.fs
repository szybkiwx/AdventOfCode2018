open Common
open System

type Position = {
    X:int;
    Y:int;
}

type Velocity = {
    X: int;
    Y: int;
}
type Point = {
    P: Position;
    V: Velocity;
}
let parseLines (lines:string list) =
    let pattern = "position=\<[ ]*(-?[0-9]{4,5}),[ ]*(-?[0-9]{4,5})\> velocity=\<([ -][0-9]), ([ -][0-9])\>"
    let rec parseLine (lines:string list) result =
        match lines with
        | first::rest ->
            match first with
            | ParseRegex pattern [Int posx; Int posy; Int vx; Int vy ] ->
                parseLine rest ( { P = { X = posx; Y = posy}; V = {X = vx; Y = vy}}::result)
        | [] -> result
    parseLine lines []

let secondPass (input: Point list) =
    let rec movePoint input result =
        match input with
        | first::rest -> movePoint rest ({P = {X = first.P.X + first.V.X ; Y = first.P.Y + first.V.Y }; V = first.V  }::result)
        | [] -> result
    movePoint input []

let calcYRange (input:Point list) =
     let ys = input |> List.map (fun x -> x.P.Y)
     (ys |> List.max) - (ys |> List.min)

let calcXRange (input:Point list) =
     let xs = input |> List.map (fun x -> x.P.X)
     (xs |> List.max) - (xs |> List.min)

let printOut (input:Point list) =
    let positions = input |> List.map (fun x -> x.P)

    let xs = positions |> List.map (fun x -> x.X)
    let maxX = xs |> List.max
    let minX = xs |> List.min

    let ys = positions |> List.map (fun x -> x.Y)

    let maxY = ys |> List.max
    let minY = ys |> List.min

    let poisitionSet = positions |> Set.ofList

    let result = [for y in minY..maxY do
                    for x in minX..maxX do
                        if poisitionSet.Contains( {X = x; Y = y} )
                        then
                            yield '#'
                        else
                            yield '.'
                    yield '\n'] |> Array.ofList |> System.String.Concat
    result

let phase1 (input:Point list) =
    let rec moveStars (positions:Point list) prevYRange i =
        let newPositions = secondPass positions

        let yRange = calcYRange newPositions

        if yRange > prevYRange  then
            (printOut positions), i
        else
            moveStars newPositions yRange (i+1)

    moveStars input Int32.MaxValue 0



[<EntryPoint>]
let main argv =
    let lines = readLines "Input.txt" |> List.ofSeq
    let input = parseLines lines
    let result1, i =  phase1 input
    printf "%s" result1
    0 // return an integer exit code
