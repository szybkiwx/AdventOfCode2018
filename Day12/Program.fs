open System
open System.Numerics
open Common
let debug (number:BigInteger) range =
    let rec loop (n:BigInteger) (result:char list) i =
        if i > range then
            result |>  String.Concat
        else
            let lastBit =  (n &&& (bigint 1))
            loop (n >>> 1) ((if lastBit.IsOne then '#' else '.')::result) (i + 1)

    loop number [] 1

let parseState str =
    str
    |> Seq.map(fun x -> bigint (if x = '#' then 1 else 0))
    |> Seq.fold(fun acc x -> (acc <<< 1) + x) (bigint 0)


let parseGeneration str =
    match str with
    | ParseRegex "([#.]{5}) => ([#.])" [stateStr; newStateStr] ->  parseState stateStr, if newStateStr = "#" then 1 else 0


let findRule (input:BigInteger) (allRules:(BigInteger*int) []) position range =
    let shift = input >>> (range - position - 5)
    let shiftedInput = shift &&& (bigint 0x1F)
    let potentialRules = [for rule in allRules do
                            if (shiftedInput ^^^ (fst rule) ).IsZero
                                then yield snd rule ]
    potentialRules  |> List.tryHead

let processGeneration (prevState:BigInteger) rules range =
    let nextGen = [ for i in 0..range-5-1 do
                    let ruleOption = findRule prevState rules i range
                    yield  match ruleOption with
                           | Some x -> x
                           | None -> 0 ]

    nextGen |> List.fold (fun acc x -> (acc <<< 1) + (bigint x)) (bigint 0) <<< 3

let phase1 (initialState:BigInteger) rules range =
    let rec loop i prevState prevRange =
        if i = 0 then
            prevState, prevRange
        else
            let nextState = processGeneration prevState rules prevRange
            printfn "%s" (debug nextState prevRange)
            if ( ( nextState >>> 5 ) ^^^ bigint 1).IsZero then
                loop (i - 1) nextState prevRange
            else
                loop (i - 1) (nextState<<<1) (prevRange + 1)


    let lastGen, newRange = loop 20 initialState range

    let rec loop (n:BigInteger) (result:char list) i =
        if i > newRange then
            result
        else
            let lastBit = (n &&& (bigint 1))
            loop (n >>> 1) ((if lastBit.IsOne then '#' else '.')::result) (i + 1)
    printfn "%s" (debug lastGen newRange)

    let result = loop lastGen [] 1

    let mutable cnt = 0
    for i in -5..newRange-5-1 do
        if result.[i + 5] = '#' then
            cnt <- cnt + i
    cnt

let countPots (generation:BigInteger) range=
    let rec loop (n:BigInteger) (result:char list) i =
        if i > range then
            result
        else
            let lastBit = (n &&& (bigint 1))
            loop (n >>> 1) ((if lastBit.IsOne then '#' else '.')::result) (i + 1)

    let result = loop generation [] 1

    let mutable cnt = 0
    for i in -5..range-5-1 do
        if result.[i + 5] = '#' then
            cnt <- cnt + i
    cnt

let phase2 (initialState:BigInteger) rules range =
    let rec loop i (history:Set<int>) prevState prevRange =
        let pots = countPots prevState prevRange
        let nextState = processGeneration prevState rules prevRange
        if history.Contains pots && i > 100 then
            i, countPots nextState (prevRange + 1)
        else
            let newHistory = history.Add pots

            if ( ( nextState >>> 5 ) ^^^ bigint 1).IsZero then
                loop (i + 1) newHistory nextState prevRange
            else
                loop (i + 1) newHistory (nextState<<<1) (prevRange + 1)


    loop 1 Set.empty initialState range



[<EntryPoint>]
let main argv =
    let input = "#.##.#.##..#.#...##...#......##..#..###..##..#.#.....##..###...#.#..#...######...#####..##....#..###"
    let data = (parseState input) <<< 5
    let inputLines = readLines "input.txt" |> Array.ofSeq |> Array.map parseGeneration
    let initialRange = input.Length + 10
    //printfn "%s" (debug data initialRange)

    //let result = phase1 data inputLines initialRange
    let _, result2 = phase2 data inputLines initialRange

    printfn "%d" result2
    0 // return an integer exit code
