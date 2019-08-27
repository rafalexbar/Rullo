module Rullo where

import Data.List (transpose)

data Rullo = Rullo {
      rRows       :: Integer
    , rColumns    :: Integer
    , rVertical   :: [Integer]
    , rHorizontal :: [Integer]
    , rPuzzle     :: [[Integer]]
} deriving (Show)

rulloGame :: Rullo
rulloGame = Rullo {
      rRows       = 8
    , rColumns    = 8
    , rVertical   = [35, 31, 43, 21, 46, 26, 20, 19]
    , rHorizontal = [42, 32, 26, 20, 37, 24, 22, 38]
    , rPuzzle     = 
        [ [9, 4, 7, 2, 1, 4, 7, 9]
        , [5, 1, 8, 2, 5, 7, 3, 3]
        , [9, 8, 4, 9, 8, 6, 7, 1]
        , [1, 5, 2, 5, 6, 8, 3, 7]
        , [6, 9, 4, 9, 8, 4, 4, 6]
        , [7, 1, 2, 1, 8, 4, 8, 8]
        , [3, 5, 4, 5, 6, 2, 5, 2]
        , [3, 4, 5, 1, 2, 1, 3, 4]
        ]
}

binary :: [[Integer]]
binary = [] : [d : b | b <- binary, d <- [0, 1]]

binary' :: Integer -> [[Integer]]
binary' n = take (2 ^ (fromInteger n)) . drop (2 ^ (fromInteger n) - 1) $ binary

combinations :: [[[Integer]]] -> [[[Integer]]]
combinations [] = [[]]
combinations (x : xs) = [x' : xs' | x' <- x, xs' <- combinations xs]

generateOptions :: [[Integer]] -> [[Integer]] -> [[[Integer]]]
generateOptions numbers = map (\row -> map (zipWith (*) row) numbers)

validOptions :: [Integer] -> [[[Integer]]] -> [[[Integer]]]
validOptions vertical puzzle' = 
    [rows' | (value, rows) <- zip vertical puzzle', let rows' = filter ((==) value . sum) rows]

generateOptions' :: [[[Integer]]] -> [[[Integer]]]
generateOptions' = map transpose . combinations

validOptions' :: [Integer] -> [[[Integer]]] -> [[[Integer]]]
validOptions' horizontal puzzle' = 
    [transpose . snd . unzip $ p' | p' <- map (zip horizontal) puzzle', all (\(value, column) -> (==) value . sum $ column) p']

solveRullo :: Rullo -> Rullo
solveRullo rullo = rullo { rPuzzle = head . solver $ rPuzzle rullo }
    where solver = validOptions' (rHorizontal rullo) . generateOptions' . validOptions (rVertical rullo) . generateOptions (binary' $ rColumns rullo)

showRullo :: Rullo -> String
showRullo rullo = unlines . map (unwords . map show) $ rPuzzle rullo

main :: IO ()
main = putStr . showRullo . solveRullo $ rulloGame
