let input = System.IO.File.ReadAllLines("./day-08-input")
printfn "Read %d lines" input.Length

let testInput = ("""be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""").Split("\n")

type Foo = {
    Inputs : string array
    Outputs: string array
}

let parseInput (s : string) =
    let inputOutput = s.Split(" | ", System.StringSplitOptions.RemoveEmptyEntries)
    let allInputs = (inputOutput.[0]).Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
    let allOutputs = (inputOutput.[1]).Split(" ", System.StringSplitOptions.RemoveEmptyEntries)

    { Inputs = allInputs; Outputs = allOutputs }

type SevenSegmentDisplay = {
    Top : char Set
    TopLeft : char Set
    TopRight : char Set
    Mid : char Set
    BottomLeft : char Set
    BottomRight : char Set
    Bottom : char Set
}

let mapPatternToSegments (p : string) =
    match p.Length with
    | 2 -> Some { Top = Set.empty; TopLeft = Set.empty; TopRight = Set.ofArray (p.ToCharArray()); Mid = Set.empty; BottomLeft = Set.empty; BottomRight = Set.ofArray (p.ToCharArray()); Bottom = Set.empty }
    | 3 -> Some { Top = Set.ofArray (p.ToCharArray()); TopLeft = Set.empty; TopRight = Set.ofArray (p.ToCharArray()); Mid = Set.empty; BottomLeft = Set.empty; BottomRight = Set.ofArray (p.ToCharArray()); Bottom = Set.empty }
    | 4 -> Some { Top = Set.empty; TopLeft = Set.ofArray (p.ToCharArray()); TopRight = Set.ofArray (p.ToCharArray()); Mid = Set.ofArray (p.ToCharArray()); BottomLeft = Set.empty; BottomRight = Set.ofArray (p.ToCharArray()); Bottom = Set.empty }
    | 7 -> Some { Top = Set.ofArray (p.ToCharArray()); TopLeft = Set.ofArray (p.ToCharArray()); TopRight = Set.ofArray (p.ToCharArray()); Mid = Set.ofArray (p.ToCharArray()); BottomLeft = Set.ofArray (p.ToCharArray()); BottomRight = Set.ofArray (p.ToCharArray()); Bottom = Set.ofArray (p.ToCharArray()) }
    | _ -> None

let matchInputsWithKnownPatterns inputs =
    inputs
    |> Array.map mapPatternToSegments
    |> Array.choose id

let aggregateSegments segments =
    let aggregateSignals (potentialSignals : char Set array) =
        let zz = potentialSignals
                 |> Array.filter (fun s -> s.Count > 0)

        if zz.Length > 0 then (zz |> Array.minBy (fun s -> s.Count)) else Set.empty

    let top = aggregateSignals (Array.map (fun s -> s.Top) segments)
    let topRight = aggregateSignals (Array.map (fun s -> s.TopRight) segments)
    let topLeft = aggregateSignals (Array.map (fun s -> s.TopLeft) segments)
    let mid = aggregateSignals (Array.map (fun s -> s.Mid) segments)
    let bottomLeft = aggregateSignals (Array.map (fun s -> s.BottomLeft) segments)
    let bottomRight = aggregateSignals (Array.map (fun s -> s.BottomRight) segments)
    let bottom = aggregateSignals (Array.map (fun s -> s.Bottom) segments)

    { Top = top; TopRight = topRight; TopLeft = topLeft; Mid = mid; BottomLeft = bottomLeft; BottomRight = bottomRight; Bottom = bottom }

let reduceSegments s =
    let top = Set.difference s.Top (Set.unionMany [s.TopLeft; s.TopRight; s.BottomRight])
    let topLeft = Set.difference s.TopLeft (Set.unionMany [top; s.TopRight; s.BottomRight])
    let mid = Set.difference s.Mid (Set.unionMany [top; s.BottomRight])
    let topRight = Set.difference s.TopRight (Set.unionMany [top; topLeft; mid])
    let bottomLeft = Set.difference s.BottomLeft (Set.unionMany [top; topRight; topLeft; mid; s.BottomRight])
    let bottomRight = Set.difference s.BottomRight (Set.unionMany [top; topLeft; mid; bottomLeft])
    let bottom = Set.difference s.Bottom (Set.unionMany [top; topRight; topLeft; mid; bottomRight])

    { Top = top; TopRight = topRight; TopLeft = topLeft; Mid = mid; BottomLeft = bottomLeft; BottomRight = bottomRight; Bottom = bottom }

let findThrees (inputs : string array) (s : SevenSegmentDisplay) =
    inputs
    |> Array.filter (fun i -> i.Length = 5)
    |> Array.filter (fun i -> (Set.intersect (Set i) s.TopRight).Count = 2)

let applyThreeToDisplay (threes : string array) (s : SevenSegmentDisplay) =
    if threes.Length = 0 then s
    else
        let signalsForThree = threes |> Array.head |> Set
        { s with TopLeft = Set.difference s.TopLeft signalsForThree
                 BottomLeft = Set.difference s.BottomLeft signalsForThree
                 Mid = Set.intersect s.Mid signalsForThree
                 Bottom = Set.intersect s.Bottom signalsForThree }

let findSixes (inputs : string array) (s: SevenSegmentDisplay) =
    inputs
    |> Array.filter (fun i -> i.Length = 6)
    |> Array.filter (fun i -> (Set.intersect (set i) s.TopRight).Count = 1)

let applySixToDisplay (sixes : string array) (s : SevenSegmentDisplay) =
    if sixes.Length = 0 then s
    else
        let signalsForSix = sixes |> Array.head |> Set
        { s with BottomRight = Set.intersect s.BottomRight signalsForSix
                 TopRight = Set.difference s.TopRight signalsForSix }

let refineModelWithKnownPatterns (inputs : string array) (s : SevenSegmentDisplay) =
    let threes = findThrees inputs s
    let modelAfterThrees = applyThreeToDisplay threes s

    let sixes = findSixes inputs modelAfterThrees
    applySixToDisplay sixes modelAfterThrees

type Segments =
     | Top
     | TopLeft
     | TopRight
     | Mid
     | BottomLeft
     | BottomRight
     | Bottom

let decodeDigit (model : SevenSegmentDisplay) (signals : string) =
    let findSegment model signal =
        if model.Top.Contains signal then Top
        else if model.TopLeft.Contains signal then TopLeft
        else if model.TopRight.Contains signal then TopRight
        else if model.Mid.Contains signal then Mid
        else if model.BottomLeft.Contains signal then BottomLeft
        else if model.BottomRight.Contains signal then BottomRight
        else Bottom

    let decodeSegments s =
        let zero = Set [ Top; TopLeft; TopRight; BottomLeft; BottomRight; Bottom ]
        let one = Set [ TopRight; BottomRight ]
        let two = Set [ Top; TopRight; Mid; BottomLeft; Bottom ]
        let three = Set [ Top; TopRight; Mid; BottomRight; Bottom ]
        let four = Set [ TopLeft; TopRight; Mid; BottomRight ]
        let five = Set [ Top; TopLeft; Mid; BottomRight; Bottom ]
        let six = Set [ Top; TopLeft; Mid; BottomLeft; BottomRight; Bottom ]
        let seven = Set [ Top; TopRight; BottomRight ]
        let eight = Set [ Top; TopLeft; TopRight; Mid; BottomLeft; BottomRight; Bottom ]
        let nine = Set [ Top; TopLeft; TopRight; Mid; BottomRight; Bottom ]

        match (Set s) with
        | n when n = zero -> 0
        | n when n = one -> 1
        | n when n = two -> 2
        | n when n = three -> 3
        | n when n = four -> 4
        | n when n = five -> 5
        | n when n = six -> 6
        | n when n = seven -> 7
        | n when n = eight -> 8
        | n when n = nine -> 9
        | _ -> failwith (sprintf "unknown segments %A" s)

    let segments = signals |> Seq.map (fun s -> findSegment model s)
    decodeSegments segments

let createModelForInputs inputs =
    matchInputsWithKnownPatterns inputs
    |> aggregateSegments
    |> reduceSegments
    |> refineModelWithKnownPatterns inputs

let convertDecodedDigitsToNumber digits =
    digits
    |> Array.fold (fun (multiplier, acc) d -> (multiplier / 10, acc + (d * multiplier))) (1000, 0)
    |> snd

(*
let singleTestPattern = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

let sampleInputs = parseInput singleTestPattern

let sampleX = createModelForInputs sampleInputs.Inputs

let answer = sampleInputs.Outputs
             |> Array.map (fun x -> decodeDigit sampleX x)
             |> convertDecodedDigitsToNumber

let models = testInput
             |> Array.map parseInput
             |> Array.map (fun f -> createModelForInputs f.Inputs)

let answers = testInput
              |> Array.map parseInput
              |> Array.zip models
              |> Array.map (fun (model, f) -> f.Outputs |> Array.map (fun o -> decodeDigit model o))
              |> Array.map convertDecodedDigitsToNumber

let sumOfAnswers = answers |> Array.sum
*)

let models = input
             |> Array.map parseInput
             |> Array.map (fun f -> createModelForInputs f.Inputs)

let answers = input
              |> Array.map parseInput
              |> Array.zip models
              |> Array.map (fun (model, f) -> f.Outputs |> Array.map (fun o -> decodeDigit model o))
              |> Array.map convertDecodedDigitsToNumber
              |> Array.sum

printfn "The final answer is %d" answers
