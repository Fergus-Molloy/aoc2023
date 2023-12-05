module Day3 where

import Data.Char hiding (isSymbol)
import Prelude hiding (getChar)

type Point = (Int, Int)

getOrdinalIndicies :: Point -> [Point]
getOrdinalIndicies (x, y) =
  filter
    (/= (x, y)) -- don't include given index
    [ (a, b)
      | a <- [p + x | p <- [-1 .. 1]],
        b <- [i + y | i <- [-1 .. 1]]
    ]

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

countWhile :: (a -> Bool) -> [a] -> Int
countWhile p as = length $ takeWhile p as

-- | drop everything to next number then return whole number + rest of string
getNextNumber :: [(Char, Point)] -> ([(Char, Point)], [(Char, Point)])
getNextNumber s = splitAt count $ drop d s
  where
    d = countWhile (not . isDigit) $ map fst s
    n = dropWhile (not . isDigit) $ map fst s
    count = countWhile isDigit n

-- | get char from lines using coords
getChar :: [String] -> Point -> Char
getChar s (x, y) = (s !! y) !! x

-- | are any of the chars next to a symbol (includes diagonals)
nextToSymbol :: [String] -> [(Char, Point)] -> Bool
nextToSymbol s = any (any (isSymbol . getChar s) . (\(_, p) -> filter inRange $ getOrdinalIndicies p))
  where
    inRange (x, y) = x >= 0 && y >= 0 && x < 10 && y < length s

-- | zip chars with thier coords
stringWCoords :: String -> [[(Char, Point)]]
stringWCoords s = zipWith (\l y -> zipWith (\c x -> (c, (x, y))) l [0 ..]) ls [0 ..]
  where
    ls = lines s

getAllNums :: [[(Char, Point)]] -> [(Char, Point)] -> [[(Char, Point)]]
getAllNums acc s = case getNextNumber s of
  (cs, []) -> cs : acc
  (cs, t) -> getAllNums (cs : acc) t

extractNums :: String -> [[(Char, Point)]]
extractNums s = concatMap (filter (/= []) . getAllNums []) (stringWCoords s)

pt1 :: String -> Either String Int
pt1 inp = Right $ sum $ map (read . foldr (\(c, _) acc -> c : acc) "") ns
  where
    ns = filter (nextToSymbol ls) $ extractNums inp
    ls = lines inp

pt2 _ = Right 0
