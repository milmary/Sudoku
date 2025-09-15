-- src/Sudoku/Solver.hs
module Sudoku.Solver where

import Sudoku.Grid
import Data.List (minimumBy)
import Data.Ord  (comparing)

-- | Kandidáti pro buňku
possibleValues :: Int -> Grid -> (Int, Int) -> [Int]
possibleValues size grid pos@(r,c)
  | grid !! r !! c /= Nothing = []
  | otherwise = [ n | n <- [1..size], isValidMove size grid pos n ]

-- | MRV heuristika: vyber prázdnou buňku s nejmenším počtem kandidátů
selectCell :: Int -> Grid -> Maybe ((Int, Int), [Int])
selectCell size grid =
  let empties = [ (pos, possibleValues size grid pos)
                | r <- [0..size-1], c <- [0..size-1]
                , let pos = (r,c), grid !! r !! c == Nothing ]
  in if null empties then Nothing
     else Just $ minimumBy (comparing (length . snd)) empties

-- | Propagace jednoduchých tahů (naked singles)
propagate :: Int -> Grid -> Grid
propagate size grid =
  case [ (pos, head cs)
       | r <- [0..size-1], c <- [0..size-1]
       , let pos = (r,c), grid !! r !! c == Nothing
       , let cs = possibleValues size grid pos, length cs == 1 ] of
    []        -> grid
    ((pos,n):_) -> propagate size (updateGrid grid pos n)

-- | Vyřešení Sudoku pomocí MRV + propagace
solve :: Int -> Grid -> Maybe Grid
solve size grid = solve' (propagate size grid)
  where
    solve' g
      | isComplete g = Just g
      | otherwise = case selectCell size g of
          Nothing         -> Nothing
          Just (pos, cs)  -> tryCandidates g pos cs
    tryCandidates _ _ [] = Nothing
    tryCandidates g pos (n:ns) =
      let g' = propagate size (updateGrid g pos n)
      in case solve' g' of
           Just sol -> Just sol
           Nothing  -> tryCandidates g pos ns

-- | Počítání řešení až do limitu (pro test jedinečnosti)
countSolutions :: Int -> Grid -> Int -> Int
countSolutions size grid limit = go (propagate size grid) 0
  where
    go g acc
      | acc >= limit  = acc
      | isComplete g  = acc + 1
      | otherwise     = case selectCell size g of
          Nothing       -> acc
          Just (pos,cs) -> loop pos cs g acc
    loop _ [] _ acc  = acc
    loop pos (n:ns) g acc =
      let acc' = go (updateGrid g pos n) acc
      in if acc' >= limit then acc' else loop pos ns g acc'
