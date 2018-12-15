// Learn more about F# at http://fsharp.org
open Common

type Point = {
    X:int; Y:int;
}
(*
type Direction =
|   Up
|   Down
|   Left
|   Right

*)
type Direction =
|   Up
|   Down
|   Left
|   Right

type IntersectionDirection =
|   IStright
|   ILeft
|   IRight

type Cart = {
    Id: int;
    Position: Point;
    Direction: Direction
    NextIntersectionDirection: IntersectionDirection
}


let mapDirectionToVector (direction:Direction) =
    match direction with
    |   Up -> (-1, 0)  //0,-1    0,1
    |   Right -> (0, 1) //1,0   -1,0
    |   Down -> (1, 0)//0,1    0,-1
    |   Left -> (0, -1) //-1,0  1,0

let mapVectorToDirection (vector:int*int) =
    match vector with
    |   (-1, 0) -> Up//0,-1    0,1
    |   (0, 1) -> Right //1,0   -1,0
    |   (1, 0) -> Down//0,1    0,-1
    |   (0, -1) -> Left //-1,0  1,0



let mapTrackTurn direction intersectionDirection =
    match intersectionDirection with
    | IStright ->  direction, IRight
    | ILeft ->  let x, y = mapDirectionToVector direction
                mapVectorToDirection (-y, x), IStright

    | IRight -> let x, y = mapDirectionToVector direction
                mapVectorToDirection (y, -x), ILeft




let zeroPad (arr:char[]) (n:int) (c:char)=
    let padSize = n - arr.Length
    let nullArray = Array.create padSize c
    [| arr; nullArray |] |> Array.concat

let readInput =
    let lines = readLines "input.txt"
    let rows = Seq.length lines
    let columns = lines |> Seq.maxBy (fun x -> x.Length) |> String.length

    lines |> Seq.toArray |> Array.map (fun x -> zeroPad (Seq.toArray x) columns ' ') |> array2D

let findCarts (data:char[,]) =
    let maxX = Array2D.length1 data
    let maxY = Array2D.length2 data
    let mutable id = 0
    [for x in 0..maxX-1 do
     for y in 0..maxY-1 do
         let char = data.[x, y]
         if char = '^' || char = '>' || char = 'v' || char = '<' then

             let dir, newVal = if char = '^' then Up, '|'
                               elif char = '>' then Right, '-'
                               elif char = 'v' then Down, '|'
                               else Left, '-'
             Array2D.set data x y newVal
             yield {Id = id; Position = {X = x; Y = y}; Direction = dir; NextIntersectionDirection = ILeft}
             id <- id + 1
    ], data

let mapBackslash direction nextIntersectionDirection=
    let vx, vy = mapDirectionToVector direction
    let next = mapVectorToDirection (vy, vx)
    next, nextIntersectionDirection

let mapSlash direction nextIntersectionDirection=
    let vx, vy = mapDirectionToVector direction
    let next = mapVectorToDirection (-vy, -vx)
    next, nextIntersectionDirection

let tick (tracks:char[,]) cartList =
    let sorted = cartList |> List.sortBy(fun x -> x.Position.X, x.Position.Y)
    [for cart in sorted do
        let {X=x; Y=y} = cart.Position
        let currentTrack = tracks.[x, y]
        let calcNext
            =   match currentTrack with
                | '+' ->  mapTrackTurn
                | '\\' -> mapBackslash
                | '/' ->  mapSlash
                | _ -> (fun x y -> x, y)

        let nextDirection, nextIntersectionDirection = calcNext cart.Direction cart.NextIntersectionDirection

        let dx, dy = mapDirectionToVector nextDirection
        let newPosition = {X = cart.Position.X + dx; Y = cart.Position.Y + dy }
        yield {Id = cart.Id; Position=newPosition; Direction=nextDirection; NextIntersectionDirection=nextIntersectionDirection }
    ]

let findCrash carts =
    carts
    |> List.sortBy(fun x -> x.Position.X, x.Position.Y)
    |> List.groupBy (fun cart -> cart.Position)
    |> List.maxBy (fun (position, cartList) -> cartList.Length)
    |> fun (position, cartList) -> position, cartList


let phase1 tracks carts =
    let rec loop prevCarts  =
        let newCarts = tick tracks prevCarts
        let position, samePosition = findCrash newCarts
        if samePosition.Length > 1 then
            position
        else
            loop newCarts
    loop carts

let containsPosition collection p =
    collection |> List.map (fun x -> x.Position) |> Set.ofList |> Set.contains p

let tick2 (tracks:char[,]) cartList =
    let sorted = cartList |> List.sortBy(fun x -> x.Position.X, x.Position.Y)

    let rec loop carts result =
        match carts with
        | cart::rest -> let {X=x; Y=y} = cart.Position
                        let currentTrack = tracks.[x, y]
                        let calcNext
                            =   match currentTrack with
                                | '+' ->  mapTrackTurn
                                | '\\' -> mapBackslash
                                | '/' ->  mapSlash
                                | _ -> (fun x y -> x, y)

                        let nextDirection, nextIntersectionDirection = calcNext cart.Direction cart.NextIntersectionDirection

                        let dx, dy = mapDirectionToVector nextDirection
                        let newPosition = {X = cart.Position.X + dx; Y = cart.Position.Y + dy }
                        if  containsPosition rest newPosition  || containsPosition result newPosition
                        then
                            let toRemove = rest@result |>  List.find(fun x -> x.Position = newPosition)
                            let next = rest |> Set.ofList |> Set.remove toRemove |> Set.toList
                            let nextResult = result |> Set.ofList |> Set.remove toRemove |> Set.toList
                            loop next nextResult
                        else
                            loop rest ({Id = cart.Id; Position=newPosition; Direction=nextDirection; NextIntersectionDirection=nextIntersectionDirection }::result )

        | [] -> result
    loop sorted []

let phase2 tracks carts =
    let rec loop prevCarts  =
        let newCarts = tick2 tracks prevCarts
        if newCarts.Length = 1 then
            newCarts |> List.map (fun x -> x.Position) |> List.head
        else
            loop newCarts
    loop carts


[<EntryPoint>]
let main argv =
    let data = readInput
    let carts,tracks = findCarts data
    //let result = phase1 tracks carts
    // \
    let test = (mapBackslash Down IStright) = (Right, IStright)
    let test2 = (mapBackslash Left IStright)= (Up, IStright)
    let test3 = (mapBackslash Right IStright)= (Down, IStright)
    let test4 = (mapBackslash Up IStright)= (Left, IStright)

    //  /
    let testt = (mapSlash Down IStright) = (Left, IStright)
    let testt2 = (mapSlash Left IStright)= (Down, IStright)
    let testt3 = (mapSlash Right IStright)= (Up, IStright)
    let testt4 = (mapSlash Up IStright)= (Right, IStright)


    let testtt = (mapTrackTurn Down ILeft) = (Right, IStright)
    let testtt2 = (mapTrackTurn Left IStright)= (Left, IRight)
    let testtt3 = (mapTrackTurn Right IRight)= (Down, ILeft)
    let testtt4 = (mapTrackTurn Up ILeft)= (Left, IStright)

    //let result = phase1 tracks carts
    let result2 = phase2 tracks carts
    0 // return an integer exit code
