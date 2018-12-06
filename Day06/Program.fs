open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}


let findBoundingBox input =
    (input |> List.map fst |> List.min, input |> List.map snd |> List.min),
    (input |> List.map fst |> List.max, input |> List.map snd |> List.max)

let calcManhattan (p1:(int*int)) (p2:(int*int)) =
    let x1, y1 = p1
    let x2, y2 = p2
    abs(x1 - x2) + abs(y1 - y2)

let phase1 (input:seq<int*int>) =
    let ((minx, miny), (maxx, maxy)) = findBoundingBox (input |> List.ofSeq)
    let grid = [ for x in minx..maxx do
                 for y in miny..maxy do
                    yield x,y ]


    grid |> List.map (fun p -> input
                                    |> Seq.map(fun c -> c, calcManhattan p c)
                                    |> Seq.groupBy snd
                                    |> Seq.sortBy fst
                                    |> Seq.head)
         |> List.filter (fun (c, dists) -> Seq.length dists = 1)
         |> List.map (fun (c, dists) -> Seq.head dists |> fst)
         |> List.filter (fun (x,y) -> x > minx || x < maxx || y > minx || y <maxy)
         |> List.countBy id
         |> List.maxBy snd


[<EntryPoint>]
let main argv =
    let data = readLines "input.txt" |> Seq.map (fun s -> s.Split [|','|]) |> Seq.map (fun arr -> int arr.[0], int arr.[1])
    let result1 = phase1 data
    printfn "%A" argv
    0 // return an integer exit code
