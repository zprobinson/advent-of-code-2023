open System

let filePath = "day1.txt"
let input = System.IO.File.ReadAllText(filePath)

let digitMap: Map<string, char> = 
    [ ("one",   '1')
    ; ("two",   '2')
    ; ("three", '3')
    ; ("four",  '4')
    ; ("five",  '5')
    ; ("six",   '6')
    ; ("seven", '7')
    ; ("eight", '8')
    ; ("nine",  '9') ]
    |> Map.ofList

let lines (str: string) = str.Split('\n')
let convertWordToDigit (word: string) =
    let keys = digitMap.Keys
    let mutable str = ""
    for i = 0 to word.Length - 1 do
        if Char.IsLetter word[i] then
            let digitFromWord = 
                keys 
                |> Seq.filter word[i..].StartsWith
                |> Seq.map (fun x -> digitMap[x])
                |> Seq.tryHead
                |> Option.map string
                |> Option.defaultValue ""

            str <- str + digitFromWord
        else
            str <- str + (word[i..i])
    str

let numbers = String.filter Char.IsDigit
let headAndTail (str: string) =
    let head = str[0]
    let tail = str[str.Length - 1]
    sprintf "%c%c" head tail

let parseCalibrationValue =
    lines 
    >> Array.map convertWordToDigit 
    >> Array.map numbers 
    >> Array.map headAndTail 
    >> Array.map int 
    >> Array.sum

printfn "%A" (parseCalibrationValue input)
