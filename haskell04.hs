-- Prática 04 de Haskell
-- Nome: Gustavo Machado de Freitas

faixaIdoso :: Int -> String
faixaIdoso x
    | 60 <= x && x <= 64 = "IDO64"
    | 65 <= x && x <= 69 = "IDO69"
    | 70 <= x && x <= 74 = "IDO74"
    | 75 <= x && x <= 79 = "IDO79"
    | 80 <= x = "IDO80"
    | otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos tuplas = [(x,y, faixaIdoso y) | (x,y) <- tuplas]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' tuplas = map (\ (x,y) -> (x,y, faixaIdoso y)) tuplas

strColor :: (Int,Int,Int) -> String
strColor (x,y,z) = "rgb(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")" 

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = [(cx + r * x,cy,r) | x <- [0..n-1]]

genReds :: Int -> [(Int,Int,Int)]
genReds n = [(x, 0, 0) | x <- take n $ cycle [1..255]]

