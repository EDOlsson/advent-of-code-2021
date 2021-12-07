let input = System.IO.File.ReadAllLines("day-06-input") |> Array.head
printfn "Read %d lines" input.Length

let processFish f =
    match f with
    | n when n <= 8 && n > 0 -> [| n - 1 |]
    | n when n = 0 -> [| 6; 8 |]
    | _ as n -> failwith (sprintf "Unexpected fish value %d" n)

let simulateTimer n fish =
    let rec runTimer n fish =
        match n with
        | 0 -> fish
        | _ -> fish |> Array.map processFish |> Array.collect id |> runTimer (n - 1)

    runTimer n fish

let startingFish = input.Split(",", System.StringSplitOptions.RemoveEmptyEntries)
                   |> Array.map (fun f -> System.Convert.ToInt32(f))

let answer = simulateTimer 80 startingFish |> Array.length
printfn "After 80 timer counts, %d" answer

let testFish = [|3;4;3;1;2|]
let testAnswer = simulateTimer 80 testFish |> Array.length
printfn "After 80 timer counts, %d (test data)" testAnswer
