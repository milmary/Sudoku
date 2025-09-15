-- src/Sudoku/Generator.hs
module Sudoku.Generator where

import Sudoku.Grid
import Sudoku.Solver (possibleValues, selectCell, countSolutions)
import System.Random (randomRIO)
import Control.Monad (forM_)
import Data.Maybe (fromJust)

-- | Náhodně promíchej seznam
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (l, y:r) = splitAt i xs
  rest <- shuffle (l ++ r)
  return (y : rest)

-- | Generuj plnou mřížku velikosti size pomocí MRV + backtrackingu
generateFullGrid :: Int -> IO Grid
generateFullGrid size = do
  mg <- fill (emptyGrid size)
  case mg of
    Just g  -> return g
    Nothing -> error "generateFullGrid: nelze vygenerovat plnou mřížku"
  where
    fill :: Grid -> IO (Maybe Grid)
    fill grid
      | isComplete grid = return (Just grid)
      | otherwise = case selectCell size grid of
          Nothing        -> return (Just grid)
          Just (pos,cs)  -> do
            cs' <- shuffle cs
            tryValues pos cs' grid

    tryValues :: (Int, Int) -> [Int] -> Grid -> IO (Maybe Grid)
    tryValues _   []     _    = return Nothing
    tryValues pos (n:ns) grid = do
      let g' = updateGrid grid pos n
      mg' <- fill g'
      case mg' of
        Just sol -> return (Just sol)
        Nothing  -> tryValues pos ns grid

-- | Obtížnost a počet buněk k odstranění
data Difficulty = Easy | Medium | Hard
  deriving (Eq, Show)

difficultyToBlanks :: Int -> Difficulty -> Int
difficultyToBlanks size Easy   = size * size `div` 4
difficultyToBlanks size Medium = size * size `div` 3
difficultyToBlanks size Hard   = size * size `div` 2

-- | Odstraň n buněk, zachovej jedinečné řešení
removeCells :: Int -> Grid -> IO Grid
removeCells 0 grid = return grid
removeCells n grid = do
  let size   = length grid
      filled = [ (r,c) | r <- [0..size-1]
                      , c <- [0..size-1]
                      , grid !! r !! c /= Nothing ]
  if null filled
    then return grid
    else do
      idx <- randomRIO (0, length filled - 1)
      let (r,c)    = filled !! idx
          modified = clearCell grid (r,c)
          sols     = countSolutions size modified 2
      if sols == 1
        then removeCells (n - 1) modified  -- zachovat odstranění: jediné řešení
        else removeCells n grid            -- rollback: více než jedno řešení

-- | Generuj Sudoku podle velikosti a obtížnosti
generateSudoku :: Int -> Difficulty -> IO Grid
generateSudoku size diff = do
  full   <- generateFullGrid size
  let blanks = difficultyToBlanks size diff
  removeCells blanks full
