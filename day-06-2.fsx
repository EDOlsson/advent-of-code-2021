let input = System.IO.File.ReadAllLines("day-06-input") |> Array.head
printfn "Read %d lines" input.Length

let processFish f =
    match f with
    | n when n <= 8 && n > 0 -> [| n - 1 |]
    | n when n = 0 -> [| 6; 8 |]
    | _ as n -> failwith (sprintf "Unexpected fish value %d" n)

let createFishMap fish =
    let groupedFish = fish
                      |> Array.groupBy id
                      |> Array.map (fun (key, theseFish) -> (key, (int64)theseFish.Length))

    Map groupedFish

let singleFishMap (fishMap : Map<int,int64>) =
    let fishAt7 = if Map.containsKey 8 fishMap then Map.find 8 fishMap else 0
    let fishAt6 = if Map.containsKey 7 fishMap then Map.find 7 fishMap else 0
    let fishAt5 = if Map.containsKey 6 fishMap then Map.find 6 fishMap else 0
    let fishAt4 = if Map.containsKey 5 fishMap then Map.find 5 fishMap else 0
    let fishAt3 = if Map.containsKey 4 fishMap then Map.find 4 fishMap else 0
    let fishAt2 = if Map.containsKey 3 fishMap then Map.find 3 fishMap else 0
    let fishAt1 = if Map.containsKey 2 fishMap then Map.find 2 fishMap else 0
    let fishAt0 = if Map.containsKey 1 fishMap then Map.find 1 fishMap else 0
    let newFish = if Map.containsKey 0 fishMap then Map.find 0 fishMap else 0

    Map [ (8, newFish)
          (7, fishAt7)
          (6, fishAt6 + newFish)
          (5, fishAt5)
          (4, fishAt4)
          (3, fishAt3)
          (2, fishAt2)
          (1, fishAt1)
          (0, fishAt0) ]

let simulateTimer n fishMap =
    let rec runTimer n fishMap =
        match n with
        | 0 -> fishMap
        | _ -> runTimer (n - 1) (singleFishMap fishMap)

    runTimer n fishMap

let startingFish = input.Split(",", System.StringSplitOptions.RemoveEmptyEntries)
                   |> Array.map (fun f -> System.Convert.ToInt32(f))

let finalFishMap = simulateTimer 256 (createFishMap startingFish)
let answer = Map.fold (fun sum _ count -> sum + count) ((int64)0) finalFishMap
printfn "After 256 timer counts, %d" answer

(*
let testFish = [|3;4;3;1;2|]
let finalFishMap = simulateTimer 256 (createFishMap testFish)
let testAnswer = Map.fold (fun sum _ count -> ((int64)sum) + ((int64)count)) ((int64)0) finalFishMap
printfn "After 256 timer counts, %d (test data)" testAnswer
printfn "After 80 days, %d (test data)" (Map.fold (fun sum _ count -> sum + count) ((int64)0) (simulateTimer 80 (createFishMap testFish)))
*)
