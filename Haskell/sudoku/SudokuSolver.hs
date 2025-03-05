import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ [sud (row, col) | col <- positions, sud (row, col) /= 0]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ [sud (row, col) | row <- positions, sud (row, col) /= 0]

findSubgrid :: (Row, Column) -> [(Row, Column)]
findSubgrid (r, c) = [(row, col) | row <- findBlock r, col <- findBlock c]
  where findBlock :: Int -> [Int]
        findBlock i = blocks !! div (i-1) 3

freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row, col) = values \\ filter (/=0) (map sud $ findSubgrid (row, col))

freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos sud (r, c) = freeInRow sud r `intersect` freeInColumn sud c
                       `intersect` freeInSubgrid sud (r, c)

openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [(row, col) | row <- positions, col <- positions, sud (row, col) == 0]

isValid :: [Value] -> Bool
isValid values = isValid' values []
  where isValid' :: [Value] -> [Value] -> Bool
        isValid' [] _ = True
        isValid' (0:xs) used = isValid' xs used
        isValid' (x:xs) used = notElem x used && isValid' xs (x:used)

rowValid :: Sudoku -> Row -> Bool
rowValid sud row = isValid [sud (row, col) | col <- positions]

colValid :: Sudoku -> Column -> Bool
colValid sud col = isValid [sud (row, col) | row <- positions]

subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid sud (row, col) = isValid $ map sud $ findSubgrid (row, col)

consistent :: Sudoku -> Bool
consistent sud = and ([rowValid sud row | row <- positions]
                      ++ [colValid sud col | col <- positions]
                      ++ [subgridValid sud (row, col)
                          | row <- centerOfBlocks, col <- centerOfBlocks])

constraints :: Sudoku -> [Constraint]
constraints sud = sortBy (\(_, _, a) (_, _, b) -> compare (length a) (length b))
                         [(row, col, freeAtPos sud (row, col))
                          | (row, col) <- openPositions sud]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Extends a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Read a file-sudoku with a Grid like format into a Sudoku.
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{- Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       -- TODO: Call your solver.
       let x = constraints sud
       print x
       printSudoku sud
