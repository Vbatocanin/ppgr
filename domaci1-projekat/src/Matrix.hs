module Matrix(
    matrixMultiply,
    extractRow,
    extractColumn
)where

-- Returns row with the specified index
extractRow :: (Num a) => [[a]] -> Int -> [a] 
extractRow matrix i =
    matrix !! i

-- Returns column with the specified index
extractColumn :: (Num a) => [[a]] -> Int -> [a] 
extractColumn matrix i =
    map (\row -> row !! i) matrix 

-- Calculates the scalar product of a single row and column
productRowColumn :: (Num a) => [a] -> [a] -> a
sumRowColumn row column =
    foldr   (\elem acc -> (fst elem) * (snd elem) + acc ) 
            0 
            (zip row column) 

matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiply m1 m2 = 
    map (\row -> 
             )
