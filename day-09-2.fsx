let testMapInput = """2199943210
3987894921
9856789892
8767896789
9899965678"""

type LocationType =
| Unknown
| Boundary
| Basin of string

type Coords = {
    Row : int
    Col : int
}

type MapLocation = {
    Location : Coords
    Type : LocationType
}

let testMapAsChars = testMapInput.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.ToCharArray())

let parseMapLocation rowIndex columnIndex c =
    match c with
    | '9' -> { Location = { Row = rowIndex; Col = columnIndex }; Type = Boundary }
    | _ -> { Location = { Row = rowIndex; Col = columnIndex }; Type = Unknown }

let parseEntireMap mapAsChars =
    mapAsChars
    |> Array.mapi (fun rowIndex row -> row
                                       |> Array.filter (fun p -> Set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> Set.contains p)
                                       |> Array.mapi (fun colIndex p -> parseMapLocation rowIndex colIndex p))
    |> Array.collect id
    |> Array.toList

let findNeighboringLocations row col availableLocations =
    availableLocations
    |> List.filter (fun loc -> loc.Location.Row = row && loc.Location.Col = col - 1
                            || loc.Location.Row = row && loc.Location.Col = col + 1
                            || loc.Location.Row = row - 1 && loc.Location.Col = col
                            || loc.Location.Row = row + 1 && loc.Location.Col = col)

let mapContainsLocation map loc =
    let count = map
                |> List.filter (fun m -> m.Location = loc)
                |> List.length

    count > 0

let findNextLocation map =
    let remainingUnknowns = map
                            |> List.filter (fun m -> m.Type = Unknown)
    if List.length remainingUnknowns > 0 then Some (remainingUnknowns |> List.minBy (fun m -> m.Location.Row)) else None

let mapBasins map =
    let rec mapBasin loc label restOfMap =
        match loc.Type with
        | Boundary -> None
        | Basin _ -> None
        | Unknown ->
            let currentBasin' = { loc with Type = Basin label }
            let restOfMap' = restOfMap |> List.filter (fun m -> m <> loc)
            let nextLocations = findNeighboringLocations loc.Location.Row loc.Location.Col restOfMap

            let mutable mapToSearch = restOfMap'
            let mutable basinSoFar = []
            for loc in nextLocations do
                let basinPortion = mapBasin loc label mapToSearch
                if basinPortion.IsSome then
                    basinSoFar <- basinSoFar |> List.append basinPortion.Value
                    mapToSearch <- restOfMap' |> List.filter (fun m -> not (mapContainsLocation basinSoFar m.Location))

            Some ((currentBasin' :: basinSoFar) |> List.distinct)

    let rec mapBasins' map basins =
        let nextLocation = findNextLocation map
        let basins' = match nextLocation with
                      | None -> basins
                      | Some loc -> let theRest = map |> List.filter (fun m -> m <> loc)
                                    let nextLabel = sprintf "%d" ((List.length basins) + 1)
                                    let basin = mapBasin loc nextLabel theRest
                                    match basin with
                                    | None -> basins
                                    | Some b -> printfn "Found %A with size %d" nextLabel b.Length
                                                b :: basins

        let flattenedBasins = basins'
                              |> List.collect id
        let map' = map
                   |> List.filter (fun m -> not (mapContainsLocation flattenedBasins m.Location))

        if nextLocation = None then
            basins'
        else
            match map' with
            | [] -> basins'
            | [ _ ] -> basins'
            | _ -> mapBasins' map' basins'

    mapBasins' map []

let calculateFinalValue basins =
    basins
    |> List.map List.length
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

let testMap = parseEntireMap testMapAsChars
let basins = testMap
             |> mapBasins

printfn "Test result : %d" (calculateFinalValue basins)

let day9Map = System.IO.File.ReadAllLines("./day-09-input")
              |> Array.map (fun s -> s.ToCharArray())
              |> Array.map (fun row -> row
                                       |> Array.filter (fun p -> Set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] |> Set.contains p))
              |> parseEntireMap

let finalBasins = day9Map
                  |> mapBasins

finalBasins
|> List.iter (fun b -> printfn "%O size : %d" b.Head.Type (List.length b))

let finalAnswer = calculateFinalValue finalBasins
printfn "Final result : %d" finalAnswer