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

//
// This ended up being written in such a way that it returned a modified state of all the bingo cards
// So the starting set of bingo cards serves as the state which gets threaded into a fold() call
// and the resulting state of bingo cards as the winning cards with their info.
//
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

let (_, completedBingoCards) = bingoNumbers
                               |> Array.fold (fun (i, c) num -> (i + 1, (applyBingoNumber i num c))) (1, bingoCards)

let lastWinningBingoCard = completedBingoCards
                           |> Seq.filter (fun c -> c.WinOrder <> -1)
                           |> Seq.maxBy (fun c -> c.WinOrder)


printfn "Winning score : %d" (calculateWinningScore lastWinningBingoCard)
