open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let day1 data = data |> Seq.map int |> Seq.sum

let day2 data =
    let rec find input acc history =
        match input with
            | [] -> find data acc history
            | head::rest -> if List.contains acc history then acc else  find rest (acc+head) (acc::history)

    find data 0 []


[<EntryPoint>]
let main argv =
    let data = readLines "input.txt"
    let ints = (Seq.toList (data |> Seq.map int))
    let result = day2 ints
    printf "%i" result
    0 // return an integer exit code
