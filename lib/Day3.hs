module Day3 where

import Data.Char hiding (isSymbol)
import Data.List
import Debug.Trace
import Prelude hiding (getChar)

type Point = (Int, Int)

-- tinp = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

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

nextToDigits :: [String] -> Point -> [Point]
nextToDigits s point = filter (isDigit . getChar s) $ filter inRange $ getOrdinalIndicies point
  where
    inRange (x, y) = x >= 0 && y >= 0 && x < w && y < h
    w = length (s !! 0)
    h = length s

-- | are any of the chars next to a symbol (includes diagonals)
nextToSymbol :: [String] -> [(Char, Point)] -> Bool
nextToSymbol s =
  any
    ( any (isSymbol . getChar s)
        . ( \(_, p) ->
              filter
                inRange
                $ getOrdinalIndicies p
          )
    )
  where
    inRange (x, y) = x >= 0 && y >= 0 && x < w && y < h
    w = length (s !! 0)
    h = length s

getGears :: [String] -> [(Char, Point)] -> [Point]
getGears s cs = concatMap (nextToGear s) cs

-- | are any of the chars next to a symbol (includes diagonals)
nextToGear :: [String] -> (Char, Point) -> [Point]
nextToGear s (_, point) = gears
  where
    gears = filter ((==) '*' . getChar s) $ filter inRange $ getOrdinalIndicies point
    inRange (x, y) = x >= 0 && y >= 0 && x < w && y < h
    w = length (s !! 0)
    h = length s

-- | zip chars with thier coords
stringWCoords :: String -> [[(Char, Point)]]
stringWCoords s = zipWith (\l y -> zipWith (\c x -> (c, (x, y))) l [0 ..]) ls [0 ..]
  where
    ls = lines s

getAllNums :: [[(Char, Point)]] -> [(Char, Point)] -> [[(Char, Point)]]
getAllNums acc s = case getNextNumber s of
  (cs, []) -> reverse (cs : acc)
  (cs, t) -> getAllNums (cs : acc) t

extractNums :: String -> [[(Char, Point)]]
extractNums s = concatMap (filter (/= []) . getAllNums []) (stringWCoords s)

-- | adjacent as in next to each other
adjacentPoints :: Point -> Point -> Bool
adjacentPoints (a, y1) (b, y2) = a - b == 1 || b - a == 1 || (a == b && y1 == y2)

getNumFromPoint :: [String] -> Point -> Int
getNumFromPoint s p = read $ getPre s p <> getPost s p
  where
    getPost ls (x, y) = takeWhile isDigit $ drop x (ls !! y)
    getPre ls (x, y) = reverse $ takeWhile isDigit $ reverse $ take x (ls !! y)

pt1 :: String -> Either String Int
pt1 inp = Right $ sum $ map (read . map fst) ns
  where
    ns = filter (nextToSymbol ls) (extractNums inp)
    ls = lines inp

duplicates :: [Point] -> [Point]
duplicates = duplicates' []

duplicates' :: [Point] -> [Point] -> [Point]
duplicates' acc [] = acc
duplicates' acc [a] = a : acc
duplicates' acc (a : ps) = if adjacentPoints a (head ps) then duplicates' acc ps else duplicates' (a : acc) ps

pt2 :: String -> Either String Int
pt2 inp = Right $ sum $ map product nums
  where
    nums = filter (\l -> length l >= 2) $ map ((nub . map (getNumFromPoint ls)) . nextToDigits ls) gears
    gears = nub $ concat $ group $ sort (concatMap (getGears ls) (extractNums inp))
    ls = lines inp
