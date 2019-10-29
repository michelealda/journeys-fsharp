open System.IO

type Direction = | North | East | South | West
type Position = (int * int * Direction)
type Move = | Forward | Left | Right
    
type Journey = { 
    startP: Position
    endP : Position
    moves: Move list
}

let parseMoves (ms: string) =
    ms.ToCharArray()
    |> List.ofArray
    |> List.choose (fun m ->
        match m with
        | 'F' -> Some Forward
        | 'R' -> Some Right
        | 'L' -> Some Left
        | _ -> None)
    |> Some
    
let parseDirection d =
    match d with
    | 'N' -> Some North
    | 'E' -> Some East
    | 'S' -> Some South
    | 'W' -> Some West
    | _ -> None

let parsePosition (x: string) =
    let xs = x.ToCharArray()
    try
       (
            xs.[0] |> string |> int,
            xs.[2] |> string |> int,
            xs.[4] |> parseDirection |> Option.get 
        ) |> Position |> Some
    with _ -> None

let parseJourney xs = 
    match xs with
    | s::m::e::_ -> 
        Option.map3 
            (fun a b c -> {startP= a; endP = b; moves=c})
            (parsePosition s) 
            (parsePosition e) 
            (parseMoves m)
    | _ -> None

let move (x, y, d) =
    match d with
    | North -> (x, y+1, d)
    | East  -> (x+1,y,d)
    | South -> (x,y-1,d)
    | West  -> (x-1,y,d)

let rotateR (x, y, d) =
    match d with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North
    |> fun newDirection -> (x,y,newDirection)

let rotateL (x, y, d) =
    match d with
    | North -> West
    | East  -> North
    | South -> East
    | West  -> South
    |> fun newDirection -> (x,y,newDirection)

let executeMove p m =
    match m with
    | Forward -> move p
    | Right -> rotateR p
    | Left -> rotateL p

let isJourneyValid j =
    match j with
    | Some x ->
        x.moves 
        |> List.fold executeMove x.startP
        |> (=) x.endP
    | None -> false

[<EntryPoint>]
let main argv =
    match argv with
    | [|f|] -> f
    | _ -> "input.txt"
    |> fun file -> File.ReadLines(file)
                   |> Seq.chunkBySize 4
                   |> Seq.map (
                        Array.toList 
                        >> parseJourney 
                        >> isJourneyValid)
                   |> Seq.iter(printfn "%A") 
    0 
