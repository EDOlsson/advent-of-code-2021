let input = System.IO.File.ReadAllLines("./day-01-input")

printfn "Read %d lines" <| input.Length

let increases = input
                |> Array.map (fun s -> System.Convert.ToInt32(s))
                |> Array.pairwise
                |> Array.filter (fun (a, b) -> a < b)
                |> Array.length

printfn "Measured %d increases" increases
