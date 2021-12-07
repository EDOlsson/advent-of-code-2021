let input = (System.IO.File.ReadAllText("./day-07-input")).Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map System.Convert.ToInt32
printfn "Read %d horizontal values" input.Length

let calculateFuelCost targetPosition currentPosition =
    let fuelCost = currentPosition - targetPosition
    if fuelCost > 0 then fuelCost else -fuelCost

let calculateTotalFuelCost targetPosition currentPositions =
    currentPositions
    |> Array.fold (fun totalCost subPosition -> totalCost + calculateFuelCost targetPosition subPosition) 0

let minFinalCost = input
                   |> Array.map (fun pos -> calculateTotalFuelCost pos input)
                   |> Array.min

printfn "Min fuel cost : %d" minFinalCost

(*
let testValues = [|16;1;2;0;4;2;7;1;2;14|]

let minTestCost = testValues
                  |> Array.map (fun pos -> calculateTotalFuelCost pos testValues)
                  |> Array.min

printfn "[Test] min fuel cost : %d" minTestCost
*)
