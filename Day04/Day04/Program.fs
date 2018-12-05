open System
open System
open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let (|Int|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


let readInput =
    let lines = readLines "input.txt"
    let rec parseDates linesToGo (linesWithDates:(DateTime * string) list) =
        match linesToGo with
        |   currentLine::rest -> match currentLine with
                                 | ParseRegex "\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.+)" [Int yr; Int mn; Int dd; Int hh; Int mm; event] ->
                                    let dt = new DateTime(yr, mn, dd, hh, mm, 0)
                                    parseDates rest ((dt, event)::linesWithDates)
        | [] -> linesWithDates
    parseDates (lines |> List.ofSeq) []

let makePairs lst =
    lst |> List.pairwise
        |> List.mapi (fun idx value -> idx % 2 = 0, value)
        |> List.filter fst
        |> List.map snd



let getNapTimes input =
    let sorted = List.sortBy (fun (dt, s) -> dt ) input

    let rec groupEvents eventsToGo currentId (currentGuardEvents:DateTime list) (eventsGrouped:Map<int, DateTime list>)  =
        match eventsToGo with
        |   current::rest ->  let dt, s = current
                              match s with
                              | ParseRegex "Guard #(\d+) begins shift" [Int id] ->
                                    if eventsGrouped.ContainsKey currentId
                                    then groupEvents rest id [] (eventsGrouped.Add(currentId, eventsGrouped.[currentId]@ (List.rev currentGuardEvents)))
                                    else groupEvents rest id [] (eventsGrouped.Add(currentId, List.rev currentGuardEvents))
                              | "wakes up" -> groupEvents rest currentId (dt::currentGuardEvents) eventsGrouped
                              | "falls asleep" -> groupEvents rest currentId (dt::currentGuardEvents) eventsGrouped
        |   [] -> eventsGrouped.Add(currentId, List.rev currentGuardEvents)

    let eventsGrouped = groupEvents sorted 0 [] Map.empty

    eventsGrouped |> Map.map (fun id lst -> makePairs lst)

let phase1 (input:(DateTime * string) list) =
    let napTimes = getNapTimes input
    let napLengths =
        napTimes |> Map.map (fun id lst -> lst |> List.fold (fun acc (s, e) -> acc + (e.Minute - s.Minute) ) 0)

    let laziestId, longestNap =
        napLengths
        |> Map.toList
        |> List.maxBy (fun (id, value) -> value)

    let mostLazyMinute, napTime =
        [   for i in 0..59 do
            for s, e in napTimes.[laziestId] do
                if i >= s.Minute && i < e.Minute
                then yield i]
        |> List.countBy (fun i -> i)
        |> List.maxBy snd
    mostLazyMinute * laziestId



let phase2 (input:(DateTime * string) list) =
    let napTimes = getNapTimes input
    for i in 0..59 do
    for guard, naps in napTimes do
    for nap in naps do


[<EntryPoint>]
let main argv =
    let input =  readInput
    //let rslt = phase1 input
    let rslt2 = phase2 input
    0 // return an integer exit code
