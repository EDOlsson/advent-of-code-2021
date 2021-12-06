let input = System.IO.File.ReadAllLines("./day-05-input")

printfn "Read %d lines" <| input.Length

type Coordinate = {
    X : int
    Y : int
}

type Line = {
    Start : Coordinate
    End : Coordinate
}

let convertInputToLine s =
    let matches = System.Text.RegularExpressions.Regex.Matches(s, "(?<x1>\d+)\,(?<y1>\d+) -> (?<x2>\d+)\,(?<y2>\d+)")

    let x1 = System.Convert.ToInt32(matches[0].Groups["x1"].Value)
    let y1 = System.Convert.ToInt32(matches[0].Groups["y1"].Value)
    let x2 = System.Convert.ToInt32(matches[0].Groups["x2"].Value)
    let y2 = System.Convert.ToInt32(matches[0].Groups["y2"].Value)

    { Start = { X = x1; Y = y1 };
      End = { X = x2; Y = y2 } }

let isLineVertical l =
    l.Start.X = l.End.X

let isLineHorizontal l =
    l.Start.Y = l.End.Y

let convertLineToCoordinates l =
    if isLineVertical l then
        if l.Start.Y <= l.End.Y then
            [| for y in l.Start.Y .. l.End.Y -> { X = l.Start.X; Y = y }|]
        else
            [| for y in l.End.Y .. l.Start.Y -> { X = l.End.X; Y = y } |]
    else if isLineHorizontal l then
        if l.Start.X < l.End.X then
            [| for x in l.Start.X .. l.End.X -> { X = x; Y = l.Start.Y }|]
        else
            [| for x in l.End.X .. l.Start.X -> { X = x; Y = l.End.Y }|]
    else
        Array.empty

let answer = input
             |> Array.map convertInputToLine
             |> Array.filter (fun l -> (isLineVertical l) || (isLineHorizontal l))
             |> Array.map convertLineToCoordinates
             |> Array.collect (id)
             |> Array.groupBy (id)
             |> Array.filter (fun (_, coords) -> coords.Length > 1)
             |> Array.length

let testData = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

let testGroups = testData.Split("\n")
                 |> Array.map convertInputToLine
                 |> Array.filter (fun l -> (isLineVertical l) || (isLineHorizontal l))
                 |> Array.map convertLineToCoordinates
                 |> Array.collect (id)
                 |> Array.groupBy (id)
                 |> Array.map (fun (key, values) -> ((sprintf "(%d,%d)" key.X key.Y), values.Length))


let testAnswer = testData.Split("\n")
                 |> Array.map convertInputToLine
                 |> Array.filter (fun l -> (isLineVertical l) || (isLineHorizontal l))
                 |> Array.map convertLineToCoordinates
                 |> Array.collect (id)
                 |> Array.groupBy (id)
                 |> Array.filter (fun (_, coords) -> coords.Length > 1)
                 |> Array.length

printfn "For test data, there are %d matching coordinates" testAnswer
