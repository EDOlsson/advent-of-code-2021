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

let countUniqueDigitsInOutput foo =
    foo.Outputs
    |> Array.filter (fun f -> f.Length = 2 || f.Length = 4 || f.Length = 3 || f.Length = 7)
    |> Array.length

let testAnswer = testInput
                 |> Array.map parseInput
                 |> Array.map countUniqueDigitsInOutput
                 |> Array.sum

printfn "For test input, found %d instances of 1, 4, 7, 8" testAnswer

let answer = input
             |> Array.map parseInput
             |> Array.map countUniqueDigitsInOutput
             |> Array.sum

printfn "Final answer: %d instances of 1, 4, 7, 8" answer
