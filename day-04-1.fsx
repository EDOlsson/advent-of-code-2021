let firstLine = System.IO.File.ReadLines("./day-04-input") |> Seq.head
let bingoNumbers = firstLine.Split(",") |> Array.map (System.Convert.ToInt32)

printfn "Read %d bingo numbers" bingoNumbers.Length

type BingoCardCell = {
    Row : int
    Col : int
    Number : int
    IsCovered : bool
}

type BingoCard = {
    Cells : BingoCardCell[]
    WinOrder : int
    FirstNumberWon : int
}

let convertToBingoCard (s : string[]) =
    let convertLineToBingoRow n (line : string) =
        line.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun v -> System.Convert.ToInt32(v.Trim()))
        |> Array.mapi (fun i num -> { Row = n; Col = i; Number = num; IsCovered = false })

    let rows = s |> Array.mapi convertLineToBingoRow |> Array.collect (fun x -> x)
    { Cells = rows; WinOrder = -1; FirstNumberWon = -1 }

let bingoCards =
    System.IO.File.ReadLines("./day-04-input")
    |> Seq.skip 2           // skip line of bingo numbers & blank line following
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.chunkBySize 5
    |> Seq.filter (fun s -> s.Length = 5)
    |> Seq.map (fun s -> convertToBingoCard s)
    |> Seq.toList

printfn "Read %d bingo cards" (bingoCards |> List.length)

let applyBingoNumber index number cards =
    let applyBingoNumberToCell num cell =
        if cell.Number = num then { cell with IsCovered = true } else cell

    let checkForBingo index number card =
        if card.WinOrder <> -1 then card else
            let checkCellsForBingo selector card =
                let coveredCells = card.Cells
                                    |> Array.filter selector
                                    |> Array.filter (fun c -> c.IsCovered)
                                    |> Array.length
                coveredCells = 5

            let checkResults = [0..4]
                                |> List.map (fun row -> checkCellsForBingo (fun c -> c.Row = row) card)
                                |> List.append ([0..4] |> List.map (fun col -> checkCellsForBingo (fun c -> c.Col = col) card))
                                |> List.filter (id)

            if checkResults.IsEmpty then card else { card with WinOrder = index; FirstNumberWon = number }


    cards
    |> Seq.map (fun c -> if c.WinOrder <> -1 then c else { c with Cells = (c.Cells |> Array.map (fun c' -> applyBingoNumberToCell number c')) })
    |> Seq.map (fun c -> checkForBingo index number c)

let calculateWinningScore (card : BingoCard) =
    let sumOfUnmarked = card.Cells
                        |> Array.filter (fun c -> not c.IsCovered)
                        |> Array.sumBy (fun c -> c.Number)

    sumOfUnmarked * card.FirstNumberWon

(*
let testData = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

let testBingoNumbers = [|7;4;9;5;11;17;23;2;0;14;21;24;10;16;13;6;15;25;12;22;18;20;8;19;3;26;1|]
let testBingoCards =
    (testData.Split("\n"))
    |> Array.skip 2           // skip line of bingo numbers & blank line following
    |> Array.filter (fun l -> l.Length > 0)
    |> Array.chunkBySize 5
    |> Array.map (fun s -> convertToBingoCard s)

let (_, completedBinboCards) = testBingoNumbers
                                |> Array.fold (fun (i,c) num -> (i + 1, (applyBingoNumber i num c))) (1, testBingoCards)

let winningTestCard = completedBinboCards
                        |> Seq.filter (fun c -> c.WinOrder <> -1)
                        |> Seq.minBy (fun c -> c.WinOrder)

// printfn "Winning test card : %A" winningTestCard
printfn "Winning score : %d" (calculateWinningScore winningTestCard)
*)

let (_, completedBingoCards) = bingoNumbers
                               |> Array.fold (fun (i, c) num -> (i + 1, (applyBingoNumber i num c))) (1, bingoCards)

let winningBingoCard = completedBingoCards
                       |> Seq.filter (fun c -> c.WinOrder <> -1)
                       |> Seq.minBy (fun c -> c.WinOrder)


printfn "Winning score : %d" (calculateWinningScore winningBingoCard)
