# Sudoku (Haskell + Threepenny GUI)

A desktop web-style app that **generates and solves Sudoku** boards of sizes **4×4**, **9×9**, and **16×16** with **Easy / Medium / Hard** difficulties.
Built in **Haskell** with a **[Threepenny-GUI](https://hackage.haskell.org/package/threepenny-gui)** front end and a clean, testable backend.

---

## Table of Contents

* [Features](#features)
* [Algorithms](#algorithms)
* [Requirements](#requirements)
* [Quick Start](#quick-start)
* [Running Tests](#running-tests)
* [Using the App](#using-the-app)

---

## Features

* **New Game** with selectable **size** (4×4, 9×9, 16×16) and **difficulty** (Easy, Medium, Hard).
* **Manual input**: cells accept only valid digits for the selected size; other characters are rejected.
* **Solve**: automatically solves the entire grid.
* **Hint (MRV)**: reveals a value for a cell chosen by the *Minimum Remaining Values* heuristic.
* **Validate**: checks if the current grid is **complete and correct**.
* **UI perks**: dynamic grid layout; chessboard block coloring for all sizes.

---

## Algorithms

* **Solver — MRV + Naked Single propagation**
  Selects the next cell with the fewest candidates (MRV), repeatedly applies **naked single** propagation, and backtracks when needed.
* **Uniqueness check**
  `countSolutions` counts solutions up to a limit to verify that a puzzle has **exactly one** solution.
* **Generator**
  Creates a full valid grid via **MRV backtracking** (with randomized candidate order), then **removes cells** while preserving uniqueness.

---

## Requirements

* **GHC** (e.g., 9.x)
* Haskell packages: **[`threepenny-gui`](https://hackage.haskell.org/package/threepenny-gui)**, **`containers`**

> Using **Cabal** or **Stack** is recommended. Direct `ghc` commands are provided for quick, ad-hoc builds.

---

## Quick Start

```bash
# from the repo root
cd project_NPP_Sudoku/src

# Build the app (direct GHC)
ghc -package containers -package threepenny-gui \
  -o sudoku Main.hs UI/GUI.hs Sudoku/Grid.hs Sudoku/Solver.hs Sudoku/Generator.hs

# Run
./sudoku
```

The UI will be available at **[http://127.0.0.1:8023/](http://127.0.0.1:8023/)**.

---

## Running Tests

```bash
cd project_NPP_Sudoku/src
ghc -package containers -o test \
  Test.hs Sudoku/Grid.hs Sudoku/Solver.hs Sudoku/Generator.hs
./test
```

---

## Using the App

1. **Choose Size & Difficulty**

   * Sizes: **4×4**, **9×9**, **16×16**
   * Difficulties: **Easy**, **Medium**, **Hard**
     Click **New Game** to generate a puzzle.

2. **Fill the Grid**

   * Each cell is an input field that accepts only digits in range (e.g., `1–4` for 4×4).
   * Pre-filled **clues** are read-only and styled differently.
   * Subgrids are colored in a **chessboard** pattern for readability.

3. **Controls**

   * **New Game** — generate a new puzzle with the current settings.
   * **Solve** — fill in a full correct solution.
   * **Hint** — pick a constrained cell via **MRV** and reveal its value.
   * **Check Solution** — verify completeness and correctness.
     Success shows a **green** message; mistakes show a **red** message.
