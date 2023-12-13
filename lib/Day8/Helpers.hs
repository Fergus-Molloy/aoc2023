module Day8.Helpers where

import SortedList

data Node = Node {name :: String, children :: (String, String)} deriving stock (Show)

-- bSearch :: SortedList [Node] -> String -> Maybe Node
-- bSearch (SortedList []) _ = Nothing
-- bSearch as a = case compare a (name middle) of
--   LT -> bSearch (Prelude.take midpoint as) a
--   GT -> bSearch (drop (midpoint + 1) as) a
--   EQ -> Just middle
--   where
--     middle = as !! midpoint
--     midpoint = floor ((fromIntegral (length as) / 2) :: Double)
