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

let isClosing c =
    match c with
    | ')' -> true
    | ']' -> true
    | '}' -> true
    | '>' -> true
    | _ -> false

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

    let x = line
            |> Seq.fold folder (None, [])

    fst x

let mapCharacterValue c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith (sprintf "Unexpected error character %c" c)

let answer = testInput
             |> Array.map findSyntaxErrors
             |> Array.choose id
             |> Array.map mapCharacterValue
             |> Array.sum

printfn "[Test] answer : %d" answer

let input = System.IO.File.ReadAllLines "./day-10-input"

let finalAnswer = input
                  |> Array.map findSyntaxErrors
                  |> Array.choose id
                  |> Array.map mapCharacterValue
                  |> Array.sum

printfn "Final answer : %d" finalAnswer
