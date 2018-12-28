open System
open Common


type Node = {
    id: int;
    d: char
}

let NullNode = {id= -1;d=' '}

let updateGraph (subBranches:Node list list) (graph:Map<Node, Node list>) (lastNode: Node) =
    [for subBranch in subBranches do
       let arr = [| subBranch |> List.toArray; [|lastNode |] |] |> Array.concat
       for i in 0..subBranch.Length - 1 do
           let adjList = graph.[arr.[i]]
           yield arr.[i], arr.[i + 1]::adjList
    ] |> List.fold (fun (acc:Map<Node, Node list>) (k, v) -> acc.Add(k, v) ) graph

(*
let rec getSubBranches lastCnt input graph =

    let rec loop  chars cnt recentBranch result =
        match chars with
        | '|'::rest -> loop rest (cnt + 1) [] ((recentBranch |> List.rev)::result)
        | ')'::rest -> let newResult = (recentBranch |> List.rev)::result
                       newResult |> List.rev, cnt
        | '('::rest -> let
                       //processBranch (rest |> List.skip (c - cnt)) [] subBranches

        |   x::rest -> loop rest (cnt + 1) ({id=cnt;d=x}::recentBranch) result
    loop input lastCnt [] []

and processBranch input lastCnt lastNode (graphOuter:Map<Node, Node list>)  =

    let rec loop chars cnt subBranches (graph: Map<Node, Node list>) =
        match chars with
        | '('::rest -> let subBranches, subCnt = getSubBranches (cnt + 1) rest graph
                       loop (rest |> List.skip (subCnt - cnt)) (subCnt + 1) subBranches graph
        |   x::rest -> let newNode = {id=cnt; d=x}
                       let newGraph =
                           if subBranches.Length > 0 then
                             [for subBranch in subBranches do
                                   let arr = [| subBranch |> List.toArray; [|newNode |] |] |> Array.concat
                                   for i in 0..subBranch.Length - 1 do
                                       let adjList = graph.[arr.[i]]
                                       yield arr.[i], arr.[i + 1]::adjList
                                ] |> List.fold (fun (acc:Map<Node, Node list>) (k, v) -> acc.Add(k, v) ) graph
                           else
                               let adjList = graph.[lastNode]
                               graph.Add(lastNode, newNode::adjList)
                       loop rest (cnt + 1) [] (newGraph.Add(newNode, []))

        | [] -> graph
    loop input lastCnt [] graphOuter *)

let rec processBranch input  =
    let rec loop chars graph =
        match chars with
        | []::rest -> graph
        |  x::rest ->
            match x with
            | '(' -> ""
            | ')' -> ""
            | _ -> dist + 1





[<EntryPoint>]
let main argv =
    //let inputString = readInput "iput.txt"
    //let inputString = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
    let inputString = "^ENWWW(NEEE|SSE(EE|N))$"
    let raw = inputString |> Seq.toList |> Seq.skip 1
    let data = raw |> Seq.take (Seq.length raw - 1) |> Seq.toList

    let result = processBranch data 0 (data@['X'])

    printfn "Hello World from F#!"
    0 // return an integer exit code
