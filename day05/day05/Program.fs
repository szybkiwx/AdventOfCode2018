open System
open System.IO

let readInput (filePath: string) =
    use sr = new StreamReader (filePath)
    sr.ReadToEnd() |>  Seq.toList

let cmp (first:char) (second:char) =
    first <> second && (Char.ToUpper first = second || Char.ToUpper second = first)

let phase1 (input:char list) =
    let rec reduce (polymer:char list) result =
        match polymer with
        | first::second::rest -> if cmp first second
                                 then reduce rest result
                                 else reduce (second::rest) (first::result)
        | first::[] -> first::result
        | [] -> result

    let rec reRun (polymer:char list) lastLen =
        let result = reduce polymer []
        let len = result |> List.length
        if len = lastLen then result else reRun result len

    reRun input input.Length |> List.length

let newInput (input:char list) small =
    let big = Char.ToUpper small
    let rec getRid toGo result =
        match toGo with
        | first::rest -> if first = big || first = small
                         then getRid rest result
                         else getRid rest (first::result)
        | [] -> result
    getRid input []

let phase2 (input:char list) =
    let newInputs = [for chr in 'a'..'z' do
                        yield phase1 (newInput input chr)]
    newInputs |> List.min


[<EntryPoint>]
let main argv =
    let input = readInput "input.txt"
    //let rslt = phase1 input
    let rslt2 = phase2 input
    0 // return an integer exit code
