open System

let getSubMatrix (matrix: float[,]) (excludeRow: int) (excludeCol: int) : float[,] =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    Array2D.init (rows - 1) (cols - 1) (fun i j ->
        let row = if i >= excludeRow then i + 1 else i
        let col = if j >= excludeCol then j + 1 else j
        matrix.[row, col])

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

let rec detFunc (matrix: float[,]) : float =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    if rows <> cols then
        failwith "Pro výpočet determinantu je potřeba čtvercová matice."
    match rows with
    | 1 -> matrix.[0, 0]
    | 2 -> matrix.[0, 0] * matrix.[1, 1] - matrix.[0, 1] * matrix.[1, 0]
    | _ ->
        [0 .. cols - 1]
        |> List.map (fun j ->
            let sign = if j % 2 = 0 then 1.0 else -1.0
            matrix.[0, j] * sign * (detFunc (getSubMatrix matrix 0 j)))
        |> List.sum

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

let rec detProc (matrix: float[,]) : float =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    if rows <> cols then
        failwith "Pro výpočet determinantu je potřeba čtvercová matice."
    if rows = 1 then
        matrix.[0, 0]
    elif rows = 2 then
        matrix.[0, 0] * matrix.[1, 1] - matrix.[0, 1] * matrix.[1, 0]
    else
        let mutable result = 0.0
        for j in 0 .. cols - 1 do
            let sign = if j % 2 = 0 then 1.0 else -1.0
            let subMatrix = getSubMatrix matrix 0 j
            result <- result + matrix.[0, j] * sign * (detProc subMatrix)
        result  

let printMatrix (label: string) (matrix: float[,]) =
    printfn "%s:" label
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    for i in 0 .. rows - 1 do
        let row = [ for j in 0 .. cols - 1 -> matrix.[i, j] ]
        printfn "  %s" (String.concat "  " (row |> List.map (sprintf "%6.1f")))

let printResults (A: float[,],B:float[,]): unit =
    printMatrix "Matice A" A
    printMatrix "Matice B" B
    printMatrix "\nSoučet matic A+B (funkcionální)" (sumMatriceFunc A B)
    printMatrix "\nSoučet matic A+B (procedurální)" (sumMatriceProc A B)
    printMatrix "\nRozdíl matic A-B (funkcionální)" (subMatriceFunc A B)
    printMatrix "\nRozdíl matic A-B (procedurální)" (subMatriceProc A B)
    printMatrix "\nSoučin matic A*B (funkcionální)" (multiMatriceFunc A B)
    printMatrix "\nSoučin matic A*B (procedurální)" (multiMatriceProc A B)
    printfn "\nDeterminant matice A (funkcionální): %f" (detFunc A)
    printfn "Determinant matice A (procedurální): %f" (detProc A)

let A:float[,] = array2D [[2; 3; 5; 5]; [1; 4; 6; 7]; [1; 5; 1; 2]; [9; 5; 3; 1]]
let B:float[,] = array2D [[5; 6; 2; 1]; [7; 8; 3; 2]; [2; 2; 3; 4]; [5; 2; 3; 7]]

printResults(A,B);

Console.ReadKey() |> ignore