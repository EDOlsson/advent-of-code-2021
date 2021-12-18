let testInput = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]""".Split("\n")

let isOpening c =
    match c with
    | '(' -> true
    | '[' -> true
    | '{' -> true
    | '<' -> true
    | _ -> false

let supplyClosingCharacter c =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | c' -> failwith (sprintf "Unexpected character : %c" c')

let isMatch openingCharacters c =
    let openingChar = openingCharacters |> List.head
    match (openingChar, c) with
    | ('(', ')') -> true
    | ('[', ']') -> true
    | ('{', '}') -> true
    | ('<', '>') -> true
    | _ -> false

let findSyntaxErrors line =
    let folder (badChar, openingCharacters) currentChar =
        match badChar with
        | Some _ -> (badChar, openingCharacters)
        | None ->
            if isOpening currentChar then
                (None, currentChar :: openingCharacters)
            else
                if isMatch openingCharacters currentChar then
                    (None, openingCharacters |> List.tail)
                else
                    (Some currentChar, openingCharacters |> List.tail)

    line
    |> Seq.fold folder (None, [])

let supplyMissingClosingCharacters openingChars =
    openingChars
    |> List.map (fun c -> supplyClosingCharacter c)

let mapCharacterValue c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> failwith (sprintf "Unexpected error character %c" c)

let calculateScore chars =
    chars
    |> List.fold (fun score c -> score * 5L + (mapCharacterValue c)) 0L

let calculateTotalScore input =
    let allScores = input
                    |> Array.map findSyntaxErrors
                    |> Array.filter (fun (c, _) -> c.IsNone)
                    |> Array.map (fun (_, chars) -> supplyMissingClosingCharacters chars)
                    |> Array.map calculateScore
                    |> Array.sort

    // we should add +1 to get the number, but arrays start at 0
    let midpoint = ((allScores |> Array.length) / 2)
    allScores.[midpoint]

let answer = calculateTotalScore testInput
printfn "[Test] answer : %d" answer

let input = System.IO.File.ReadAllLines "./day-10-input"

let finalAnswer = calculateTotalScore input
printfn "Final answer : %d" finalAnswer
