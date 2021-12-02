let input = System.IO.File.ReadAllLines("./day-02-input")

printfn "Read %d lines" <| input.Length

type DirectionType =
    | Forward
    | Up
    | Down

type Instruction =
    { Direction : DirectionType
      Quantity : int64 }

let parseInstruction (s : string) =
    let parts = s.Split(' ')

    let parseDirection (s : string) =
        match s.ToUpper() with
            | "FORWARD" -> Forward
            | "DOWN" -> Down
            | "UP" -> Up
            | _ -> failwith <| sprintf "Unknown direction type %s" s

    { Direction = (parseDirection parts.[0]); Quantity = (System.Convert.ToInt64(parts.[1])) }

type Position =
    { Horizontal : int64
      Depth : int64 }

let processInstruction pos i =
    match i with
    | { Direction = Forward; Quantity = n } -> { Horizontal = pos.Horizontal + n; Depth = pos.Depth }
    | { Direction = Up; Quantity = n } -> { Horizontal = pos.Horizontal; Depth = pos.Depth - n }
    | { Direction = Down; Quantity = n } -> { Horizontal = pos.Horizontal; Depth = pos.Depth + n }

let finalPosition = input
                    |> Array.map parseInstruction
                    |> Array.fold processInstruction { Horizontal = 0; Depth = 0 }

printfn "Final position :\r\n%A\tProduct : %d" finalPosition (finalPosition.Horizontal * finalPosition.Depth)
