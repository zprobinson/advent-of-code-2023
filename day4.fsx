open System

let filePath = "day4.txt"
let input = System.IO.File.ReadAllText(filePath)

let input' = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

let lines (s: string) = s.Split('\n')

let (|CardId|_|) (line: string) =
    if line.StartsWith("Card ") then
        let colonIndex = line.IndexOf(':')
        let cardId = line[5..colonIndex - 1]
        Some (cardId, line[colonIndex + 2..])
    else
        None

let parseNumbers (line: string) =
    line.Split('|')
    |> Array.map (fun s -> s.Split(' '))
    |> Array.map (Array.filter (fun s -> s <> ""))
    |> fun sx -> (sx[0], sx[1])

let findWinnings (tuple: string array * string array) =
    let winning, mine = tuple
    let matching = 
        winning
        |> Array.filter (fun x -> Array.contains x mine)

    matching

let valueOfCard (matching: string array) =
    matching
    |> Array.length
    |> float
    |> fun x ->
        let power = x - 1.0
        if power < 0 then 0.0
        else Math.Pow(2.0, x - 1.0)

let execute =
    lines
    >> Array.choose (function | CardId (id, line) -> Some (id, line) | _ -> None)
    >> Array.map (fun (id, numbers) -> parseNumbers numbers)
    >> Array.map findWinnings
    >> Array.map valueOfCard
    >> Array.sum

let result = execute input
printfn "%A" result
