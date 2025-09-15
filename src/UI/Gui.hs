-- src/UI/Gui.hs
module UI.GUI where

import Sudoku.Grid
import Sudoku.Solver (solve, selectCell)
import Sudoku.Generator (generateSudoku, Difficulty(..))
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (forM, forM_, void)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.List (transpose)
import Data.Char (isDigit, digitToInt)

-- | Spuštění GUI aplikace
start :: IO ()
start = startGUI defaultConfig { jsStatic = Just "static", jsPort = Just 8023 } setup

-- | Hlavní konfigurace okna
setup :: Window -> UI ()
setup window = do
  -- Nastavení názvu okna
  return window # set title "Sudoku GUI"
  
  -- Pomocné funkce pro výběr velikosti a obtížnosti
  let mkSizeOption s = UI.option # set text (show s ++ "×" ++ show s)
                                 # set UI.value (show s)
      mkDiffOption (lbl, val) = UI.option # set text lbl
                                      # set UI.value val
      readDifficulty :: String -> Difficulty
      readDifficulty "Easy"   = Easy
      readDifficulty "Medium" = Medium
      readDifficulty "Hard"   = Hard
      readDifficulty _        = Medium

  -- Selektor velikosti a obtížnosti
  sizeSel <- UI.select #+ map mkSizeOption [4,9,16]
  difficultySel <- UI.select #+ map mkDiffOption [("Snadná","Easy"),
                                                 ("Střední","Medium"),
                                                 ("Těžká","Hard")]

  -- Ovládací tlačítka
  newBtn   <- UI.button #+ [string "Nová hra"]
  solveBtn <- UI.button #+ [string "Vyřešit"]
  hintBtn  <- UI.button #+ [string "Nápověda"]
  checkBtn <- UI.button #+ [string "Zkontroluj řešení"]

  -- Popisek pro stavové zprávy
  statusLbl <- UI.span # set UI.text ""

  -- Kontejner pro mřížku
  gridContainer <- UI.div

  -- Stavové reference (velikost, mřížka, buňky)
  sizeRef  <- liftIO $ newIORef (9 :: Int)
  gridRef  <- liftIO $ generateSudoku 9 Medium >>= newIORef
  cellsRef <- liftIO $ newIORef ([] :: [[UI.Element]])

  -- Funkce pro vykreslení mřížky podle modelu
  let buildGrid :: Int -> Grid -> UI [[UI.Element]]
      buildGrid size grid = do
        -- Vymazání předchozí mřížky
        element gridContainer # set children []

        -- Vytvoření vstupních polí
        cells <- forM [0..size-1] $ \r ->
          forM [0..size-1] $ \c -> do
            let v = fmap show (grid !! r !! c)
                valStr = fromMaybe "" v
            inp <- UI.input
              # set (attr "maxlength") (if size < 10 then "1" else "2")
              # set UI.value valStr
              # set UI.enabled (grid !! r !! c == Nothing)
              # set UI.style [ ("width",  cellSize size)
                             , ("height", cellSize size)
                             , ("text-align", "center")
                             , ("font-size", fontSize size)
                             , ("border", "1px solid #999")
                             , ("outline", "none") ]

            -- Omezení na platné číslice
            on UI.keyup inp $ \_ -> do
              txt <- get UI.value inp
              let filtered = filter isDigit txt
                  newVal = case filtered of
                    [] -> ""
                    xs -> let d = digitToInt (head xs)
                          in if d >= 1 && d <= size then show d else ""
              element inp # set UI.value newVal

            -- Barvení bloků šachovnicově
            let bSize = floor . sqrt $ fromIntegral size
                blk   = (r `div` bSize) * bSize + (c `div` bSize)
                darkBlocks = case size of
                  4  -> [0,3]
                  9  -> [1,3,5,7]
                  16 -> [0,2,5,7,8,10,13,15]
                  _  -> []
                color = if blk `elem` darkBlocks
                        then "#e0f0ff"
                        else "#c0e0f8"
            element inp # set UI.style [("background-color", color)]
            return inp

        -- Rozložení polí v CSS gridu
        layout <- UI.div
          # set UI.style [ ("display", "grid")
                         , ("grid-template-columns",
                            unwords (replicate size (cellSize size)))
                         , ("gap", "0px")
                         , ("margin", "10px 0")
                         , ("width", "max-content") ]
          #+ concatMap (map element) cells
        element gridContainer #+ [element layout]
        return cells

      -- Pomocné funkce pro rozměry polí
      cellSize n = show (max 20 (200 `div` n)) ++ "px"
      fontSize n = show (max 12 (180 `div` n)) ++ "px"

      -- Aktualizace UI podle modelu
      updateUI :: Int -> Grid -> [[UI.Element]] -> UI ()
      updateUI size grid cells = forM_ (zip [0..] cells) $ \(r,row) ->
        forM_ (zip [0..] row) $ \(c,el) -> do
          let v = fmap show (grid !! r !! c)
          void $ element el # set UI.value (fromMaybe "" v)
          void $ element el # set UI.enabled (grid !! r !! c == Nothing)

      -- Čtení UI do modelu
      readGrid :: Int -> [[UI.Element]] -> UI Grid
      readGrid size cells = sequence [ sequence [ parse <$> get UI.value el
                                               | el <- row ]
                                     | row <- cells ]
        where parse txt = if null txt then Nothing else Just (read txt)

      -- Ověření správnosti mřížky
      isValidGrid :: Int -> Grid -> Bool
      isValidGrid size g = all valid (rows ++ cols ++ blocks)
        where
          rows = g
          cols = transpose g
          bSize = floor . sqrt $ fromIntegral size
          blocks = [ [ g !! r !! c
                     | r <- [br*bSize..br*bSize+bSize-1]
                     , c <- [bc*bSize..bc*bSize+bSize-1] ]
                   | br <- [0..bSize-1], bc <- [0..bSize-1] ]
          valid xs = let ys = [n | Just n <- xs] in length ys == length (unique ys)
          unique = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

  -- Počáteční vykreslení mřížky
  initSize  <- liftIO (readIORef sizeRef)
  initGrid  <- liftIO (readIORef gridRef)
  initCells <- buildGrid initSize initGrid
  liftIO $ writeIORef cellsRef initCells

  -- Obsluha tlačítka "Nová hra"
  on UI.click newBtn $ \_ -> do
    sVal <- get UI.value sizeSel
    dVal <- get UI.value difficultySel
    let size = read sVal
        diff = readDifficulty dVal
    g  <- liftIO (generateSudoku size diff)
    liftIO $ writeIORef sizeRef size
    liftIO $ writeIORef gridRef g
    cs <- buildGrid size g
    liftIO $ writeIORef cellsRef cs
    void $ element statusLbl # set UI.text ""

  -- Obsluha tlačítka "Vyřešit"
  on UI.click solveBtn $ \_ -> do
    size <- liftIO (readIORef sizeRef)
    cs   <- liftIO (readIORef cellsRef)
    g    <- readGrid size cs
    case solve size g of
      Just sol -> do
        liftIO $ writeIORef gridRef sol
        updateUI size sol cs
      Nothing -> do
        void $ element statusLbl # set UI.text "Nelze vyřešit."
        void $ element statusLbl # set UI.style [("color","red")]

  -- Obsluha tlačítka "Nápověda"
  on UI.click hintBtn $ \_ -> do
    size <- liftIO (readIORef sizeRef)
    cs   <- liftIO (readIORef cellsRef)
    g    <- readGrid size cs
    case selectCell size g of
      Just ((r,c),n:_) -> void $ element (cs !! r !! c) # set UI.value (show n)
      _                -> pure ()

  -- Obsluha tlačítka "Zkontroluj řešení"
  on UI.click checkBtn $ \_ -> do
    size <- liftIO (readIORef sizeRef)
    cs   <- liftIO (readIORef cellsRef)
    g    <- readGrid size cs
    if isComplete g && isValidGrid size g
      then do
        void $ element statusLbl # set UI.text "Gratulujeme! Řešení je správné."
        void $ element statusLbl # set UI.style [("color","green")]
      else do
        void $ element statusLbl # set UI.text "Mřížka není správně vyplněná."
        void $ element statusLbl # set UI.style [("color","red")]

  -- Sestavení hlavního layoutu
  void $ getBody window #+ 
    [ UI.div  #+ [ string "Velikost: ", element sizeSel
                 , string " Obtížnost: ", element difficultySel
                 , element newBtn ]
    , UI.div  #+ [ element solveBtn, element hintBtn, element checkBtn ]
    , element gridContainer
    , element statusLbl
    ]
