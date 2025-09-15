-- src/Test.hs
module Main where

import Sudoku.Grid
import Sudoku.Solver (solve, selectCell, countSolutions)
import Sudoku.Generator (generateFullGrid, generateSudoku, Difficulty(..), difficultyToBlanks)
import Data.Maybe (isJust)
import Control.Monad (forM_, void)

-- Jednoduchý pomocník pro aserce výsledků testů
data Result = OK | FAIL deriving Show
assert :: Bool -> String -> IO Result
assert cond msg = do
  let res = if cond then OK else FAIL
  putStrLn $ msg ++ ": " ++ show res
  return res

main :: IO ()
main = do
  putStrLn "Running Sudoku tests..."

  ----------------------------------------
  -- 1) Testy generování úplných mřížek
  ----------------------------------------
  -- 4×4
  g4 <- generateFullGrid 4
  void $ assert (length g4 == 4 && all ((==4) . length) g4)
    "generateFullGrid 4: correct dimensions"
  void $ assert (isComplete g4) "generateFullGrid 4: is complete"

  -- 9×9
  g9 <- generateFullGrid 9
  void $ assert (length g9 == 9 && all ((==9) . length) g9)
    "generateFullGrid 9: correct dimensions"
  void $ assert (isComplete g9) "generateFullGrid 9: is complete"

  -- 16×16
  g16 <- generateFullGrid 16
  void $ assert (length g16 == 16 && all ((==16) . length) g16)
    "generateFullGrid 16: correct dimensions"
  void $ assert (isComplete g16) "generateFullGrid 16: is complete"

  ----------------------------------------
  -- 2) Test identického řešení na plných mřížkách
  ----------------------------------------
  void $ assert (solve 4 g4 == Just g4)   "solve: identity on 4x4 full grid"
  void $ assert (solve 9 g9 == Just g9)   "solve: identity on 9x9 full grid"
  void $ assert (solve 16 g16 == Just g16) "solve: identity on 16x16 full grid"

  ----------------------------------------
  -- 3) Testy více řešení pomocí countSolutions
  ----------------------------------------
  void $ assert (countSolutions 4 (emptyGrid 4) 2 >= 2)
    "countSolutions: empty 4x4 has multiple solutions"
  void $ assert (countSolutions 9 (emptyGrid 9) 2 >= 2)
    "countSolutions: empty 9x9 has multiple solutions"

  ----------------------------------------
  -- 4) Test přesného řešení známé hádanky
  ----------------------------------------
  let puzzle4 = [ [Just 1,Nothing,Just 4,Nothing]
                , [Nothing,Just 3,Nothing,Just 1]
                , [Nothing,Just 1,Nothing,Just 4]
                , [Nothing,Just 4,Just 1,Nothing] ]
      solution4 = Just
        [ [Just 1,Just 2,Just 4,Just 3]
        , [Just 4,Just 3,Just 2,Just 1]
        , [Just 2,Just 1,Just 3,Just 4]
        , [Just 3,Just 4,Just 1,Just 2] ]
  void $ assert (solve 4 puzzle4 == solution4)
    "solve: known 4x4 puzzle exact match"

  ----------------------------------------
  -- 5) Testy vygenerovaných hádanek podle velikosti a obtížnosti
  ----------------------------------------
  let combos = [(4,Easy),(4,Medium),(4,Hard)
               ,(9,Easy),(9,Medium),(9,Hard)
               ,(16,Easy),(16,Medium),(16,Hard)]
  forM_ combos $ \(sz,diff) -> do
    p <- generateSudoku sz diff
    -- jedinečné řešení
    void $ assert (countSolutions sz p 2 == 1)
      ("generateSudoku " ++ show sz ++ " " ++ show diff ++ ": unique solution")
    -- řešitelná
    void $ assert (isJust (solve sz p))
      ("solve " ++ show sz ++ " " ++ show diff ++ " puzzle solvable")
    -- správný počet prázdných polí
    let blankCnt = length [ () | row <- p, cell <- row, cell == Nothing ]
        expected = difficultyToBlanks sz diff
    void $ assert (blankCnt == expected)
      ("generateSudoku " ++ show sz ++ " " ++ show diff ++ " blanks count correct")

  putStrLn "All tests completed."
