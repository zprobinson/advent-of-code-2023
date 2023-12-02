open System

let filePath = "day2.txt"
let input = System.IO.File.ReadAllText(filePath)
// let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

let lines (input: string) = input.Split('\n')

let (|GameId|_|) (line: string) =
    if line.StartsWith("Game ") then
        let colonIndex = line.IndexOf(':')
        let gameId = line[5..colonIndex - 1]
        Some (gameId, line[colonIndex + 2..])
    else
        None
let parseRounds (line: string) =
    line.Split(';')
    |> Array.map (fun s -> s.Trim())

type Round = { Blue: int; Red: int; Green: int }
module Round =
    let isImpossible (round: Round) =
        round.Red > 12 ||
        round.Green > 13 ||
        round.Blue > 14

    let maximize (rounds: Round array) =
        let maxRed = rounds |> Array.maxBy (fun r -> r.Red)
        let maxGreen = rounds |> Array.maxBy (fun r -> r.Green)
        let maxBlue = rounds |> Array.maxBy (fun r -> r.Blue)
        { Blue = maxBlue.Blue; Red = maxRed.Red; Green = maxGreen.Green }

    let power (round: Round) =
        round.Red * round.Blue * round.Green

let getColor (color: string) =
    Array.filter (fun (s: string) -> s.Contains(color))
    >> Array.tryHead
    >> Option.map (fun s -> 
        s.ToCharArray() 
        |> Array.filter Char.IsDigit 
        |> fun c -> new string(c))
    >> Option.map int
    >> Option.defaultValue 0
let parseRound (round: string) =
    let cubes = round.Split(',')
    let red = cubes |> getColor "red"
    let blue = cubes |> getColor "blue"
    let green = cubes |> getColor "green"
    { Blue = blue; Red = red; Green = green }

let parseGameId (line: string) =
    match line with
    | GameId (gameId, rest) -> 
        let rounds = 
            rest 
            |> parseRounds 
            |> Array.map parseRound
        (int gameId, rounds)
    | _ -> failwith "Invalid input"

let playGame =
    lines
    >> Array.map parseGameId
    // >> Array.filter (fun (_, rounds) -> 
    //     Array.exists Round.isImpossible rounds |> not)
    >> Array.map (snd >> Round.maximize)
    >> Array.map Round.power
    // >> Array.map fst
    >> Array.sum
    >> printfn "%i"
    // >> Array.iter (printfn "%A")

do playGame input