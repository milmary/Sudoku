-- src/Sudoku/Grid.hs
module Sudoku.Grid where

import Data.List (transpose)

-- | Jeden prázdný nebo vyplněný čtverec
type Cell = Maybe Int
-- | Mřížka velikosti NxN
type Grid = [[Cell]]

-- | Inicializuj prázdnou mřížku dané velikosti
emptyGrid :: Int -> Grid
emptyGrid size = replicate size (replicate size Nothing)

-- | Vypiš mřížku do konzole
prettyPrint :: Int -> Grid -> IO ()
prettyPrint size grid =
  putStrLn $ unlines [unwords (map formatCell row) | row <- grid]
  where
    formatCell Nothing  = "."
    formatCell (Just n) = show n

-- | Ověří, že vložení čísla n na pozici (row,col) neporuší pravidla
isValidMove :: Int -> Grid -> (Int, Int) -> Int -> Bool
isValidMove size grid (row, col) n =
  notElem (Just n) (getRow grid row)
    && notElem (Just n) (getCol grid col)
    && notElem (Just n) (getBlock size grid (row, col))

-- | Získá řádek r
getRow :: Grid -> Int -> [Cell]
getRow grid r = grid !! r

-- | Získá sloupec c
getCol :: Grid -> Int -> [Cell]
getCol grid c = map (!! c) grid

-- | Získá blok (sub-grid) odpovídající pozici (r,c)
getBlock :: Int -> Grid -> (Int, Int) -> [Cell]
getBlock size grid (r, c) =
  [ grid !! r' !! c'
  | r' <- [br * bSize .. br * bSize + bSize - 1]
  , c' <- [bc * bSize .. bc * bSize + bSize - 1]
  ]
  where
    bSize = floor . sqrt $ fromIntegral size
    br = r `div` bSize
    bc = c `div` bSize

-- | Nastaví buňku (r,c) na hodnotu n
updateGrid :: Grid -> (Int, Int) -> Int -> Grid
updateGrid grid (r, c) n =
  take r grid
    ++ [ take c (grid !! r) ++ [Just n] ++ drop (c + 1) (grid !! r) ]
    ++ drop (r + 1) grid

-- | Vymaže buňku (r,c)
clearCell :: Grid -> (Int, Int) -> Grid
clearCell grid (r, c) =
  take r grid
    ++ [ take c (grid !! r) ++ [Nothing] ++ drop (c + 1) (grid !! r) ]
    ++ drop (r + 1) grid

-- | Kontrola, zda je mřížka kompletní (žádné Nothing)
isComplete :: Grid -> Bool
isComplete = all (all (/= Nothing))

-- | Najdi první prázdnou buňku
findEmpty :: Grid -> Maybe (Int, Int)
findEmpty grid =
  case [ (r, c)
       | r <- [0 .. length grid - 1]
       , c <- [0 .. length grid - 1]
       , grid !! r !! c == Nothing
       ] of
    []    -> Nothing
    (x:_) -> Just x