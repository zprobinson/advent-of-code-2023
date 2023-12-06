open System

let filePath = "day3.txt"
// let input = System.IO.File.ReadAllText(filePath)
let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

let lines (s: string) = s.Split('\n')
let charArray (s: string) = s.ToCharArray()
let isSymbol (c: char) = not <| Char.IsLetterOrDigit(c) && c <> '.'

type Point = { row: int; col: int }
type Symbol = { value: char; point: Point }
let get2dIndex (cs: char array array) =
    let mutable targets: Symbol list = []
    for i in 0 .. cs.Length - 1 do
        for j in 0 .. cs.[i].Length - 1 do
            let value = cs[i][j]
            if isSymbol value then
                targets <- { value = value; point = { row = i; col = j } } :: targets
            else
                ()

    targets

type Candidates = { symbol: Symbol; candidates: Point list }
let findCandidates (cs: char array array) (symbol: Symbol) =
    let parseCandidateLocations (point: Point) =
        [ (point.row - 1, point.col - 1)
          (point.row - 1, point.col)
          (point.row - 1, point.col + 1)
          (point.row, point.col - 1)
          (point.row, point.col + 1)
          (point.row + 1, point.col - 1)
          (point.row + 1, point.col)
          (point.row + 1, point.col + 1) ]
        |> List.filter (fun (row, col) -> row >= 0 && col >= 0 && row < cs.Length && col < cs[row].Length)
        |> List.map (fun (row, col) -> { row = row; col = col })
        |> fun candidates -> { symbol = symbol; candidates = candidates }

    symbol.point |> parseCandidateLocations
    
type PartNumberPartial = { value: char; point: Point }
type PartNumber = { partNumber: string; points: Point list }
module PartNumber =
    let fromPartNumberPartial (partial: PartNumberPartial seq) =
        let partNumber = partial |> Seq.map (fun p -> p.value) |> Seq.toArray |> String
        let points = partial |> Seq.map (fun p -> p.point) |> List.ofSeq
        { partNumber = partNumber; points = points }

let findPartNumber (cs: char array array) (candidates: Candidates) =
    let partNumbers = cs |> Array.map (fun row -> row |> String)
    let result =
        cs
        |> Array.mapi (fun i row -> row |> Array.mapi (fun j c -> { value = c; point = { row = i; col = j } }))
        // |> Array.fold (fun acc  )

    candidates.candidates
    |> List.map (fun candidate -> cs[candidate.row][candidate.col])
    |> List.filter (fun c -> Char.IsDigit(c))
    |> List.map (string >> int)

let execute (input: string) =
    let chars = input |> lines |> Array.map charArray

    let symbols = chars |> get2dIndex |> List.rev

    symbols
    |> List.map (findCandidates chars)
    |> List.map (findPartNumber chars)
    |> List.map (sprintf "symbol found: %A")
    |> List.reduce (fun acc x -> acc + "\n" + x)

let result = execute input

printfn "%s" result