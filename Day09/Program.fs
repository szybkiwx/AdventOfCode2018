open System
open System.Collections.Generic

//[1; 2; 3; 4] 1 5
//[1; 2; 3] [5] [4]`

let insertMarble (circle: List<int>) currentMarblePos newMarble =
    //[| circle.[0..currentMarblePos + 1] ; [| newMarble|] ; circle.[(currentMarblePos + 2) ..] |] |>Array.concat
    if circle.Count = 1 then
        circle.Add(newMarble)
        circle, 1
    elif currentMarblePos = circle.Count - 1 then
        circle.Insert(1, newMarble)
        circle, 1
    elif currentMarblePos =  circle.Count - 2 then
        circle.Add(newMarble)
        circle, circle.Count
    else
        circle.Insert(currentMarblePos + 1, newMarble)
        circle, currentMarblePos + 2

let removeOnPosition (circle: int[]) positionToRemove =
    let removed = circle.[positionToRemove]
    [| circle.[0..positionToRemove - 1]; circle.[positionToRemove + 1 ..]|] |> Array.concat, removed


let calcLeft (circle: List<int>)  pos n =
    if pos >= n then pos - n
    else  circle.Count - n + pos

let phase1 players lastMarbleValue =
    let rec nextRound (scores:Map<int,int>)  (player:int) marbles currentMarblePos marbleNumber  =
        if marbleNumber > lastMarbleValue
        then
            scores
        else
            let newMarble = marbleNumber + 1

            let nextPlayer = if (player + 1) % (players + 1) = 0 then 1 else player + 1
            if newMarble % 23 <> 0 then
                let newMarbles, newMarblePos = insertMarble marbles currentMarblePos newMarble
                nextRound scores  nextPlayer newMarbles newMarblePos newMarble
            else
                let positionToRemove = calcLeft marbles currentMarblePos 7 // ? can exceed array?
                let removed = marbles.[positionToRemove]
                marbles.RemoveAt(positionToRemove);
                let currentScore = if scores.ContainsKey player then scores.[player] else 0
                let newScores = scores.Add(player, currentScore + newMarble + removed)
                let newCurrent = if positionToRemove < marbles.Count - 1 then positionToRemove else marbles.Count - 1
                nextRound newScores  nextPlayer marbles newCurrent newMarble

    let init = new List<int>()
    init.Add(0)
    let scores = nextRound Map.empty 0 init 0 0
    scores |> Map.toList |> List.sortByDescending snd |> List.head |> snd

[<EntryPoint>]
let main argv =

    let test = phase1 10 1618
    let result1 = phase1 403 71920

    0 // return an integer exit code
