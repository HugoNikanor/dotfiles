module Data.List.Split (splitOn) where

import Data.List (isPrefixOf)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn key lst = case span (/= key) lst of
        (x, []) -> [x]
        (x, xs) -> x : splitOn key xs
