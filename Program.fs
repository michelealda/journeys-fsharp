open System
open System.IO

type Direction = | North | East | South | West
type Move = | Forward | Left | Right

type Position = {
     x: int
     y: int
     direction: Direction
 }

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

let tryInt (s: string) =
    match Int32.TryParse(s) with
    | (true,int) -> Some int
    | _ -> None
    
let parsePosition (x: string) =
    let xs = x.ToCharArray()
    Option.map3
        (fun x y direction -> {x=x;y=y;direction=direction})
        (xs.[0] |> string |> tryInt)
        (xs.[2] |> string |> tryInt)
        (xs.[4] |> parseDirection)

let parseJourney xs = 
    match xs with
    | s::m::e::_ -> 
        Option.map3 
            (fun a b c -> {startP= a; endP = b; moves=c})
            (parsePosition s) 
            (parsePosition e) 
            (parseMoves m)
    | _ -> None

let move (p:Position) =
    match p.direction with
    | North -> {p with y= p.y+1}
    | East  -> {p with x= p.x+1}
    | South -> {p with y= p.y-1}
    | West  -> {p with x= p.x-1}

let toTheRight d = 
    match d with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North

let toTheLeft d = 
    match d with
    | North -> West
    | East  -> North
    | South -> East
    | West  -> South

let rotate rotateFn (p:Position) = {p with direction = (rotateFn p.direction)}

let executeMove p m =
    p |> match m with
         | Forward -> move 
         | Right -> rotate toTheRight 
         | Left -> rotate toTheLeft 

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
