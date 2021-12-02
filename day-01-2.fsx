let input = System.IO.File.ReadAllLines("./day-01-input")

printfn "Read %d lines" <| input.Length

let increases = input
                |> Array.map System.Convert.ToInt32
                |> Array.windowed 3                         // create sliding window 3 elements wide
                |> Array.map (fun win -> Array.sum win)     // add up the values in each window
                |> Array.pairwise                           // select pairs of sums
                |> Array.filter (fun (a, b) -> a < b)       // filter out decreases (only allow increases)
                |> Array.length

printfn "Detected %d increases (3-measurement window)" increases
