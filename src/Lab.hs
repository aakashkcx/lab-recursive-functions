--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive functions                                                   --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, subsequences )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
elem n []     = False
elem n (x:xs)
    | n == x    = True
    | otherwise = elem n xs

maximum :: Ord a => [a] -> a
maximum [x]    = x
maximum (x:xs) = max x (maximum xs)

intersperse :: a -> [a] -> [a]
intersperse _ [x]    = [x]
intersperse _ []     = []
intersperse y (x:xs) = x : y : intersperse y xs

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

--------------------------------------------------------------------------------
