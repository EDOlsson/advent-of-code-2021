let input = System.IO.File.ReadAllLines("./day-03-input")

printfn "Read %d lines" <| input.Length

type BitCount = {
    Zeroes : int
    Ones : int
}

let countBitsInWord (w : string) =
    w
    |> Seq.map (fun c -> if c = '0' then { Zeroes = 1; Ones = 0 } else { Zeroes = 0; Ones = 1 })
    |> Seq.toArray

type RatingType =
    | OxygenGenerator
    | Co2Scrubber

let filterByRating (bc : BitCount) (r : RatingType) =
    match r with
    | OxygenGenerator -> if bc.Zeroes > bc.Ones then '0' else '1'
    | Co2Scrubber -> if bc.Ones < bc.Zeroes then '1' else '0'

let accumulateBitCounts (values : string[]) =
    values
    |> Array.map countBitsInWord
    |> Array.reduce (fun c1 c2 -> c1 |> Array.mapi (fun i bit -> { Zeroes = bit.Zeroes + c2.[i].Zeroes; Ones = bit.Ones + c2.[i].Ones }))

let findOxygenRating (values : string[]) =
    let rec applyFilter (currentFilter : char[]) (values : string[]) =
        match values with
        | [|v|] -> v
        | _ as remainingValues -> 
            let x = accumulateBitCounts remainingValues
            let filter' = x |> Array.map (fun x' -> filterByRating x' OxygenGenerator)
            let updatedFilter = Array.append currentFilter [|filter'.[currentFilter.Length]|]
            let filteredValues = values |> Array.filter (fun v -> v.StartsWith(new string(updatedFilter)))
            applyFilter updatedFilter filteredValues

    applyFilter [||] values

let findCo2Rating (values : string[]) =
    let rec applyFilter (currentFilter : char[]) (values : string[]) =
        match values with
        | [|v|] -> v
        | _ as remainingValues -> 
            let x = accumulateBitCounts remainingValues
            let filter' = x |> Array.map (fun x' -> filterByRating x' Co2Scrubber)
            let updatedFilter = Array.append currentFilter [|filter'.[currentFilter.Length]|]
            let filteredValues = values |> Array.filter (fun v -> v.StartsWith(new string(updatedFilter)))
            applyFilter updatedFilter filteredValues

    applyFilter [||] values

let convertWordToNumber s =
    s
    |> Seq.fold (fun n c -> if c = '0' then n <<< 1 else n <<< 1 ||| 1) 0

let oxygenGeneratorRating = findOxygenRating input
let co2ScrubberRating = findCo2Rating input

printfn "O2 Generator Rating : %s\t%d" oxygenGeneratorRating (convertWordToNumber oxygenGeneratorRating)
printfn "CO2 Scrubber Rating : %s\t%d" co2ScrubberRating (convertWordToNumber co2ScrubberRating)
printfn "Lift Support rating : %d" ((convertWordToNumber oxygenGeneratorRating) * (convertWordToNumber co2ScrubberRating))

(*
let testData = [|
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
|]

let oxygenGeneratorRating = findOxygenRating testData
let co2ScrubberRating = findCo2Rating testData

printfn "Test O2 generator rating : %s" oxygenGeneratorRating
printfn "Test O2 generator value : %d" (convertWordToNumber oxygenGeneratorRating)

printfn "Test CO2 scrubber rating : %s" co2ScrubberRating
printfn "Test CO2 scrubber value : %d" (convertWordToNumber co2ScrubberRating)
*)