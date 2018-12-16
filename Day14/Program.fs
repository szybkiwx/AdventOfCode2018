open System

let movePointer (sequence:int list) current n =
    let length = sequence.Length
    (current + length - n) % length |> abs


let appendNewValue lst value =
    if value < 10 then
        value::lst
    else
        (value % 10)::1::lst


let phase1 runs =
    let start = 7::3::[]

    let rec loop lastIndex1 lastIndex2 (result:int list) cnt =
        let value1 = result.[lastIndex1]
        let value2 = result.[lastIndex2]
        let newValue = value1 + value2
        let newResult = appendNewValue result newValue

        let shift = newResult.Length - result.Length

        let next1 = movePointer newResult (lastIndex1 + shift) (1 + value1)
        let next2 = movePointer newResult (lastIndex2 + shift) (1 + value2)

        if cnt = runs + 10 then
            result
        else
          loop next1 next2 newResult (cnt + 1)

    loop 1 0 start 0 |> List.rev |> List.skip runs |> List.take 10 |> String.Concat



[<EntryPoint>]
let main argv =
    let result1 = phase1 990941
    0 // return an integer exit code
