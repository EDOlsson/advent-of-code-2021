let input = (System.IO.File.ReadAllText("./day-07-input")).Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map System.Convert.ToInt32
printfn "Read %d horizontal values" input.Length

let calculateFuelCost targetPosition currentPosition =
    let rec accumulateFuelCost targetPosition consumedFuel nextFuelCost currentPosition =
        match currentPosition with
        | n when n = targetPosition -> consumedFuel
        | _ -> accumulateFuelCost targetPosition (consumedFuel + nextFuelCost) (nextFuelCost + 1) (currentPosition + 1)

    if currentPosition < targetPosition then
        accumulateFuelCost targetPosition 0 1 currentPosition
    else
        accumulateFuelCost currentPosition 0 1 targetPosition

let calculateTotalFuelCost targetPosition currentPositions =
    currentPositions
    |> Array.fold (fun totalCost subPosition -> totalCost + calculateFuelCost targetPosition subPosition) 0

let possiblePositions = [| (Array.min input)..(Array.max input) |]
let minFinalCost = possiblePositions
                   |> Array.map (fun pos -> calculateTotalFuelCost pos input)
                   |> Array.min

printfn "Min fuel cost : %d" minFinalCost

(*
let testValues = [|16;1;2;0;4;2;7;1;2;14|]
let possiblePositions = [| (Array.min testValues)..(Array.max testValues)|]

let minTestCost = possiblePositions
                  |> Array.map (fun pos -> calculateTotalFuelCost pos testValues)
                  |> Array.min

printfn "[Test] min fuel cost : %d" minTestCost
*)
