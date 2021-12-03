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

let convertBitCountsToGammaRate (bitCounts : BitCount[]) =
    bitCounts
    |> Array.fold (fun n c -> if c.Zeroes > c.Ones then n <<< 1 else n <<< 1 ||| 1) 0

let convertBitCountsToEpsilonRate (bitCounts : BitCount[]) =
    bitCounts
    |> Array.fold (fun n c -> if c.Zeroes > c.Ones then n <<< 1 ||| 1 else n <<< 1) 0

let finalBitCount = input
                    |> Array.map countBitsInWord
                    |> Array.reduce (fun c1 c2 -> c1 |> Array.mapi (fun i bit -> { Zeroes = bit.Zeroes + c2.[i].Zeroes; Ones = bit.Ones + c2.[i].Ones }))

let gammaRate = convertBitCountsToGammaRate finalBitCount
let epsilonRate = convertBitCountsToEpsilonRate finalBitCount

printfn "Gamma rate : %d and Epsilon rate : %d\r\nProduct : %d" gammaRate epsilonRate ((int64)gammaRate * (int64)epsilonRate)

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

let testBitCount = testData
                   |> Array.map countBitsInWord
                   |> Array.reduce (fun c1 c2 -> c1 |> Array.mapi (fun i bit -> { Zeroes = bit.Zeroes + c2.[i].Zeroes; Ones = bit.Ones + c2.[i].Ones }))

printfn "Test bit count : %A" testBitCount
printfn "Test gamma rate : %d" (convertBitCountsToGammaRate testBitCount)
printfn "Test epsilon rate : %d" (convertBitCountsToEpsilonRate testBitCount)
