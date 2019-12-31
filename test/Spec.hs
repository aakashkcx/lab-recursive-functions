--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Recursive functions                                                   --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.List (nub,sort)

import qualified Lab as L

--------------------------------------------------------------------------------

separatedBy :: Eq a => a -> [a] -> Bool
separatedBy s []       = True
separatedBy s [y]      = True
separatedBy s (_:y:xs) =
    not (null xs) && y == s && separatedBy s xs

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "elem" $
        prop "determines whether elements belong to a list" $
            \(xs :: [Int]) -> \x -> L.elem x xs == not (null (filter (==x) xs))
    describe "maximum" $
        prop "finds the maximum in non-empty lists" $
            forAll (listOf1 arbitrary) $ \(xs :: [Int]) ->
            L.maximum xs == last (sort xs)
    describe "intersperse" $
        prop "separates elements of a list with some value" $
            \(xs :: [Char]) (x :: Char) ->
            separatedBy x (L.intersperse x xs)
    describe "subsequences" $ modifyMaxSize (const 10) $ do
        prop "produces the right number of subsequences" $ \(xs :: [Int]) ->
            length (L.subsequences xs) == 2^(length xs)
        prop "produces unique subsequences" $ \(xs :: [Int]) ->
            length (L.subsequences (nub xs)) ==
            length (nub (L.subsequences (nub xs)))

--------------------------------------------------------------------------------
