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
elem _ []     = False 
elem a (x:xs) = a==x || elem a xs 

elem' :: Eq a => a -> [a] -> Bool
-- elem' a = foldr (\x r -> a==x || r) False
elem' a = foldl (\r x -> a==x || r) False

elem'' :: Eq a => a -> [a] -> Bool
elem'' needle = not . null . filter (==needle) 

maximum :: Ord a => [a] -> a
maximum [x] = x 
-- maximum (x:xs) = let r = maximum xs 
--                  in max x r
maximum (x:xs) = max x r
    where r = maximum xs 

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max 

-- intersperse ';' "abc" => "a;b;c"
intersperse :: a -> [a] -> [a]
intersperse sep [] = []
intersperse sep [x] = [x]
intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)

-- subsequences "abc" => ["", "a", "b", "c", "ab", "bc", "ac", "abc"]
-- subsequences "ab" => ["", "a", "b", "ab"]
-- subsequences "a" => ["", "a"]
-- subsequences "" => [""]
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = ys ++ map (x:) ys
    where ys = subsequences xs

--    subsequences ['a', 'b']
-- => ys ++ map ('a':) ys 
--    where ys = subsequences ['b']
-- => ys ++ map ('a':) ys 
--    where ys = ys' ++ map ('b':) ys'
--          ys' = subsequences []
-- => ys ++ map ('a':) ys 
--    where ys = ys' ++ map ('b':) ys'
--          ys' = [[]]
-- => ys ++ map ('a':) ys 
--    where ys = [[]] ++ map ('b':) [[]]
-- => ys ++ map ('a':) ys 
--    where ys = [[]] ++ [['b']]
-- => ys ++ map ('a':) ys 
--    where ys = [[], ['b']]
-- => [[], ['b']] ++ map ('a':) [[], ['b']] 
-- => [[], ['b']] ++ [['a'], ['a', 'b']] 
-- => [[], ['b'], ['a'], ['a', 'b']] 

--------------------------------------------------------------------------------
