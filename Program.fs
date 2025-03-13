open System

let A:float[,] = array2D [[2; 3; 5]; [1; 4; 6]; [1;5;1]]
let B:float[,] = array2D [[5; 6; 2]; [7; 8; 3]; [2;2;3]]

let sumMatriceFunc (A: float[,]) (B: float[,]):float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro sčítání."
    else
        Array2D.init rows cols (fun i j -> A.[i, j] + B.[i, j])

let subMatriceFunc (A: float[,]) (B: float[,]):float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro odečítání."
    else
        Array2D.init rows cols (fun i j -> A.[i, j] - B.[i, j])

let multiMatriceFunc (A: float[,]) (B: float[,]):float[,] =
    let rowsA = A.GetLength(0)
    let colsA = A.GetLength(1)
    let rowsB = B.GetLength(0)
    let colsB = B.GetLength(1)
    if colsA <> rowsB then
        failwith "Při násobení musí být počet sloupců v první matici roven počtu řádků ve druhé matici."
    else
        Array2D.init rowsA colsB (fun i j ->
            Array.init colsA (fun k -> A.[i, k] * B.[k, j]) |> Array.sum)

let sumMatriceProc (A: float[,]) (B: float[,]): float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro sčítání."
    else
        let result = Array2D.zeroCreate<float> rows cols
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                result.[i, j] <- A.[i, j] + B.[i, j]
        result

let subMatriceProc (A: float[,]) (B: float[,]): float[,] =
    let rows = A.GetLength(0)
    let cols = A.GetLength(1)
    if rows <> B.GetLength(0) || cols <> B.GetLength(1) then
        failwith "Matice musí mít stejné rozměry pro odečítání."
    else
        let result = Array2D.zeroCreate<float> rows cols
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                result.[i, j] <- A.[i, j] - B.[i, j]
        result

let multiMatriceProc (A: float[,]) (B: float[,]): float[,] =
    let rowsA = A.GetLength(0)
    let colsA = A.GetLength(1)
    let rowsB = B.GetLength(0)
    let colsB = B.GetLength(1)
    if colsA <> rowsB then
        failwith "Při násobení musí být počet sloupců v první matici roven počtu řádků ve druhé matici."
    else
        let result = Array2D.zeroCreate<float> rowsA colsB
        for i in 0 .. rowsA - 1 do
            for j in 0 .. colsB - 1 do
                let mutable sum = 0.0
                for k in 0 .. colsA - 1 do
                    sum <- sum + A.[i, k] * B.[k, j]
                result.[i, j] <- sum
        result

let printMatrix (label: string) (matrix: float[,]) =
    printfn "%s:" label
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        let row = [ for j in 0 .. cols - 1 -> matrix.[i, j] ]
        printfn "  %s" (String.concat "  " (row |> List.map (sprintf "%6.1f")))

printMatrix "Matice A" A
printMatrix "Matice B" B
printMatrix "Součet matic A+B (funkcionální)" (sumMatriceFunc A B)
printMatrix "Součet matic A+B (procedurální)" (sumMatriceProc A B)
printMatrix "Rozdíl matic A-B (funkcionální)" (subMatriceFunc A B)
printMatrix "Rozdíl matic A-B (procedurální)" (subMatriceProc A B)
printMatrix "Součin matic A*B (funkcionální)" (multiMatriceFunc A B)
printMatrix "Součin matic A*B (procedurální)" (multiMatriceProc A B)

Console.ReadKey() |> ignore