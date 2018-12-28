// Learn more about F# at http://fsharp.org

open System
let checkForA() =
    let mutable b = 0
    let mutable c = 0
    let mutable d = 0
    let mutable cnt = 0
    let mutable v = []
    let rec loop() =
        c <- d ||| 65536
        d <- 6152285
        let rec innerLoop() =

            b <- c &&& 255
            d <- b + d
            d <- d &&& 16777215
            d <- d * 65899
            d <- d &&& 16777215
            if c < 256 then
                d
            else
                c <- int (floor (float c / float 256)) //+ 1
                innerLoop()
        innerLoop()
        if v |> Set.ofList |> Set.contains d then
            v |> List.head
        else
           v <- d::v
           loop()
    loop()
let phase1() =  checkForA()

[<EntryPoint>]
let main argv =
    let result = phase1()
    0 // return an integer exit code
