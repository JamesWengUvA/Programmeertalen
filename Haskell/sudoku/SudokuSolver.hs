{-
Name: James Weng
UvAnetID: 15685365
Study: BSc Informatica

This file reads and solves a sudoku by first reading a sudoku and printing the solution.
It also accepts nrc sudoku's and will apply the rules during solving.
-}

import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])
type Solver = Sudoku -> Maybe Sudoku

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

greyBlocks :: [[Int]]
greyBlocks = [[2..4], [6..8]]

centerOfSubgrids :: [Int]
centerOfSubgrids = [2, 5, 8]

centerOfGreyBlocks :: [Int]
centerOfGreyBlocks = [3, 7]

freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ [sud (row, col) | col <- positions, sud (row, col) /= 0]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ [sud (row, col) | row <- positions, sud (row, col) /= 0]

findSubgrid :: (Row, Column) -> [(Row, Column)]
findSubgrid (r, c) = [(row, col) | row <- findBlock r, col <- findBlock c]
  where
    findBlock :: Int -> [Int]
    findBlock i = blocks !! div (i-1) 3

freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud pos = values \\ filter (/=0) (map sud $ findSubgrid pos)

findGreyBlock :: (Row, Column) -> Maybe [(Row, Column)]
findGreyBlock (r, c)
    | inGrey r && inGrey c = Just [(row, col) | row <- rowBlock, col <- colBlock]
    | otherwise = Nothing
  where
    inGrey x = x `elem` concat greyBlocks
    rowBlock = if r <= 4 then head greyBlocks else last greyBlocks
    colBlock = if c <= 4 then head greyBlocks else last greyBlocks

freeInGrey :: Sudoku -> (Row, Column) -> [Value]
freeInGrey sud (row, col) = case findGreyBlock (row, col) of
    Nothing -> values
    Just block -> values \\ filter (/= 0) (map sud block)

openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [(row, col) | row <- positions, col <- positions, sud (row, col) == 0]

isValid :: [Value] -> Bool
isValid values = isValid' (filter (/=0) values) []
  where
    isValid' :: [Value] -> [Value] -> Bool
    isValid' [] _ = True
    isValid' (x:xs) used = notElem x used && isValid' xs (x:used)

rowValid :: Sudoku -> Row -> Bool
rowValid sud row = isValid [sud (row, col) | col <- positions]

colValid :: Sudoku -> Column -> Bool
colValid sud col = isValid [sud (row, col) | row <- positions]

subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid sud (row, col) = isValid $ map sud $ findSubgrid (row, col)

greyBlockValid :: Sudoku -> (Row, Column) -> Bool
greyBlockValid sud (row, col) = case findGreyBlock (row, col) of
    Nothing -> True
    Just block -> isValid $ map sud block

solver :: (Sudoku -> (Row, Column) -> [Value]) -> (Sudoku -> Bool) -> Solver
solver freeAtPosImpl consistentImpl = solveSudoku
  where
    solveSudoku :: Sudoku -> Maybe Sudoku
    solveSudoku sud | null (openPositions sud) = if consistentImpl sud then Just sud else Nothing
                    | otherwise = case constraints sud of
                         ((row, col, values):_) -> tryValues sud row col values

    constraints :: Sudoku -> [Constraint]
    constraints sud = sortBy
        (\(_, _, a) (_, _, b) -> compare (length a) (length b))
        [(row, col, freeAtPosImpl sud (row, col)) | (row, col) <- openPositions sud]

    tryValues :: Sudoku -> Row -> Column -> [Value] -> Maybe Sudoku
    tryValues _ _ _ [] = Nothing
    tryValues sud row col (value:vs) =
        let newSud = extend sud (row, col, value) in
            if consistentImpl newSud
            then case solveSudoku newSud of
                Just result -> Just result
                Nothing -> tryValues sud row col vs
            else tryValues sud row col vs

normalSolver :: Solver
normalSolver = solver normalFreeAtPos normalConsistent
  where
    normalFreeAtPos :: Sudoku -> (Row, Column) -> [Value]
    normalFreeAtPos sud (r, c) =
        freeInRow sud r `intersect` freeInColumn sud c
        `intersect` freeInSubgrid sud (r, c)

    normalConsistent :: Sudoku -> Bool
    normalConsistent sud = and
        ([rowValid sud row | row <- positions] ++ [colValid sud col | col <- positions]
        ++ [subgridValid sud (row, col)
            | row <- centerOfSubgrids, col <- centerOfSubgrids])

nrcSolver :: Solver
nrcSolver = solver nrcFreeAtPos nrcConsistent
  where
    nrcFreeAtPos :: Sudoku -> (Row, Column) -> [Value]
    nrcFreeAtPos sud (r, c) =
        freeInRow sud r `intersect` freeInColumn sud c
        `intersect` freeInSubgrid sud (r, c) `intersect` freeInGrey sud (r, c)

    nrcConsistent :: Sudoku -> Bool
    nrcConsistent sud = and
        ([rowValid sud row | row <- positions] ++ [colValid sud col | col <- positions]
        ++ [subgridValid sud (row, col)
            | row <- centerOfSubgrids, col <- centerOfSubgrids]
        ++ [greyBlockValid sud (row, col)
            | row <- centerOfGreyBlocks, col <- centerOfGreyBlocks])

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

getSolver :: [String] -> Solver
getSolver (_:"nrc":_) = nrcSolver
getSolver _ = normalSolver

main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       let solver = getSolver args
       case solver sud of
          Just solution -> printSudoku solution
          Nothing -> error "No solution found"
