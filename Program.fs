open System

let A:float[,] = array2D [[2; 3]; [1; 4]]
let B:float[,] = array2D [[5; 6]; [7; 8]]

let sumMatrice (A: float[,]) (B: float[,]):float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro sčítání."
    else
        Array2D.init rows cols (fun i j -> A.[i, j] + B.[i, j])

let subtractMatrice (A: float[,]) (B: float[,]):float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro odečítání."
    else
        Array2D.init rows cols (fun i j -> A.[i, j] - B.[i, j])

printfn "Matice A:\n %A" A
printfn "Matice B:\n %A" B
printfn "Součet matic A+B:\n %A" (sumMatrice A B)
printfn "Rozdíl matic A-B:\n %A" (subtractMatrice A B)

Console.ReadKey() |> ignore