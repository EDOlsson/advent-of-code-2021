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
        if l.Start.X <= l.End.X then
            [| for x in l.Start.X .. l.End.X -> { X = x; Y = l.Start.Y }|]
        else
            [| for x in l.End.X .. l.Start.X -> { X = x; Y = l.End.Y }|]
    else // assume 45-degree diagonal per problem
        let slope = (double)(l.End.Y - l.Start.Y) / (double)(l.End.X - l.Start.X)
        let yAddend = if slope > 0 then 1 else -1
        let xBegin = System.Math.Min(l.Start.X, l.End.X)
        let steps = System.Math.Max(l.End.X - l.Start.X, 1)
        [| for i in 0..steps -> { X = xBegin + i; Y = l.Start.Y + (i * yAddend) } |]

let answer = input
             |> Array.map convertInputToLine
             |> Array.map convertLineToCoordinates
             |> Array.collect (id)
             |> Array.groupBy (id)
             |> Array.filter (fun (_, coords) -> coords.Length > 1)
             |> Array.length

printfn "There are %d overlapping coordinates" answer
