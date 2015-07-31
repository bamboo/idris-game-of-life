module Main

import Data.SortedSet

Cell : Type
Cell = (Int, Int)

Cells : Type
Cells = SortedSet Cell

cells : Cells -> List Cell
cells = Data.SortedSet.toList

isAlive : Cells -> Cell -> Bool
isAlive = flip contains

merge : Cells -> Cells -> Cells
merge xs ys = foldl (flip insert) xs ys

DList : (a : Type) -> Type
DList a = List a -> List a

segment : Nat -> List a -> List (List a)
segment n xs = segment' ([]++) (split xs)
  where split : List a -> (List a, List a)
        split = splitAt n
        segment' : DList (List a) -> (List a, List a) -> List (List a)
        segment' acc (x, []) = acc [x]
        segment' acc (x, xs) = segment' ((acc [x])++) (split xs)

dims : Cells -> (Int, Int)
dims = foldl max' (0, 0)
  where max' (x, y) (x', y') = (max x (x' + 1), max y (y' + 1))

grid : (Int, Int) -> Cells -> List (List Char)
grid (w, h) cs = segment (toNat w) [cell x y | y <- [0..(h - 1)], x <- [0..(w - 1)]]
   where cell x y = if isAlive cs (x, y)
                       then '#'
                       else ' '

instance Show Cells where
  show cs = unlines $ map pack $ grid (dims cs) cs

instance Eq Cells where
  xs == ys = cells xs == cells ys

-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by overcrowding.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
neighbours : Cell -> List Cell
neighbours (x, y) = [(x,     y - 1), (x,     y + 1),
                     (x - 1, y - 1), (x + 1, y - 1),
                     (x - 1, y),     (x + 1, y),
                     (x - 1, y + 1), (x + 1, y + 1)]

liveNeighbours : Cells -> Cell -> List Cell
liveNeighbours cs = filter (isAlive cs) . neighbours

liveNeighboursLength : Cells -> Cell -> Nat
liveNeighboursLength cs = length . liveNeighbours cs

surviving : Cells -> Cells
surviving cs = fromList $ filter survivor (cells cs)
  where survivor c = let n = liveNeighboursLength cs c
                     in n == 2 || n == 3

dead : Cells -> List Cell
dead cs = filter (not . isAlive cs) allNeighbours
  where allNeighbours = nub $ concatMap neighbours (cells cs)

newborn : Cells -> Cells
newborn cs = fromList $ filter ((== 3) . liveNeighboursLength cs) (dead cs) 

tick : Cells -> Cells
tick cells = surviving cells `merge` newborn cells

flicker : Cells
flicker = fromList [(1, 1), (0, 1), (2, 1)]

gosperGun : Cells
gosperGun = fromList [
  (1,5), (2,5), (1,6), (2,6),
  (35,3), (36,3), (35,4), (36,4),
  (11,5), (11,6), (11,7), (12,4), (12,8),
  (13,3), (14,3), (13,9), (14,9), (15,6),
  (16,4), (16,8), (17,5), (17,7), (17,6), (18,6),
  (21,3), (21,4), (21,5), (22,3), (22,4), (22,5),
  (23,2), (23,6), (25,2), (25,1), (25,6), (25,7)]

pp : Cells -> IO ()
pp = putStrLn . show

main : IO ()
main = loop gosperGun
  where loop : Cells -> IO ()
        loop cs = do
          pp cs
          let cs' = tick cs
          when (cs' /= cs) $
            loop cs'

