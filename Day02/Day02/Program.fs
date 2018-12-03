open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


let getRepetitions (str:string) =
    let rec scan s (freqMap:Map<char, int>) =
        match s with
        |   current::rest ->
                if freqMap.ContainsKey current
                then
                    let newVal = freqMap.[current] + 1
                    let newMap = freqMap.Add(current, newVal)
                    scan rest newMap
                else
                    scan rest (freqMap.Add(current, 1))
        |   [] -> let two = freqMap |> Map.exists (fun _ (x:int) -> x = 2 ) |> fun x -> if x then 1 else 0
                  let three = freqMap |> Map.exists (fun _ (x:int) -> x = 3 ) |> fun x -> if x then 1 else 0
                  two, three

    scan (str |> Seq.toList) Map.empty

let day1 (data: string list) =
    let rec goThrough (strings: string list) two three =
        match strings with
        |   current::rest ->
                let (t2, t3) = getRepetitions current
                goThrough rest (two + t2) (three + t3)
        |   [] -> two * three

    goThrough data 0 0


let cmp (str1:string) (str2:string) =
    let arr1 = str1 |> Seq.toArray
    let arr2 = str2 |> Seq.toArray
    let mutable cnt = 0

    for i in 0 .. arr1.Length - 1 do
        cnt <- cnt + (if arr1.[i] = arr2.[i] then 0 else 1)
    cnt

let getResult (first:string) (second:string) =
    let arr1 = first |> Seq.toArray
    let arr2 = second |> Seq.toArray
    let mutable result = ""

    for i in 0 .. arr1.Length - 1 do
        if arr1.[i] = arr2.[i] then
            result <- result + string arr1.[i]
    result

let day2 (data: string list) =
     let cartesian = [ for a in data do
                       for b in data do
                       yield (a, b) ]

     let rec scan pairs =
        match pairs with
        |   (first, second)::rest -> if cmp first second = 1 then getResult first second else scan rest
        |   [] -> ""

     scan cartesian


[<EntryPoint>]
let main argv =
    let data = readLines "input.txt" |> Seq.toList
    let result1 = day1 data
    let result2 = day2 data
    0 // return an integer exit code