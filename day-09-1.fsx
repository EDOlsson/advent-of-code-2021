let testMapInput = """2199943210
3987894921
9856789892
8767896789
9899965678"""

type LowPoint = { Point : int }
type AdjacentPoint = { Row : int; Col : int }
type Candidate = { Value : int; Neighbors : AdjacentPoint array }

let testMapAsChars = testMapInput.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.ToCharArray())

let testMap = testMapAsChars
              |> Array.map (fun row -> row
                                       |> Array.filter (fun p -> Set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> Set.contains p)
                                       |> Array.map (fun p -> sprintf "%c" p |> int))

let createCandidate p rowIndex colIndex =
        { Value = p
          Neighbors = [| { Row = rowIndex; Col = colIndex - 1}
                         { Row = rowIndex - 1; Col = colIndex }
                         { Row = rowIndex; Col = colIndex + 1 }
                         { Row = rowIndex + 1; Col = colIndex } |] }

let filterValidAdjacentPoints (entireMap : int array array) candidate =
    { candidate with Neighbors = candidate.Neighbors
                                 |> Array.filter (fun p -> p.Row >= 0 && p.Row < entireMap.Length && p.Col >= 0 && p.Col < entireMap.[0].Length) }

let isLowPoint (entireMap : int array array) candidate =
    let adjacentValues = candidate.Neighbors
                         |> Array.map (fun n -> entireMap.[n.Row].[n.Col])

    candidate.Value < Array.min adjacentValues

let findLowPointsInRow map rowIndex row =
    row
    |> Array.mapi (fun colIndex p -> createCandidate p rowIndex colIndex)
    |> Array.map (fun candidate -> filterValidAdjacentPoints map candidate)
    |> Array.filter (fun candidate -> isLowPoint map candidate)
    |> Array.map (fun lowPoint -> lowPoint.Value)

let mapLowValueToRiskLevel v =
    v + 1

let lowPoints = testMap
                |> Array.mapi (fun rowIndex row -> findLowPointsInRow testMap rowIndex row)
                |> Array.collect id
                |> Array.map mapLowValueToRiskLevel
                |> Array.sum

let day9Map = System.IO.File.ReadAllLines("./day-09-input")
              |> Array.map (fun s -> s.ToCharArray())
              |> Array.map (fun row -> row
                                       |> Array.filter (fun p -> Set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> Set.contains p)
                                       |> Array.map (fun p -> (sprintf "%c" p) |> int))

let finalRiskValue = day9Map
                     |> Array.mapi (fun rowIndex row -> findLowPointsInRow day9Map rowIndex row)
                     |> Array.collect id
                     |> Array.map mapLowValueToRiskLevel
                     |> Array.sum

printfn "Final risk value : %d" finalRiskValue
