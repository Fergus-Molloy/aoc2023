module Day6 where

import AocParse

parseTimes :: Parser [Int]
parseTimes = matchString "Time:" *> matchWhiteSpace *> splitOn matchWhiteSpace matchInt

parseDistances :: Parser [Int]
parseDistances = matchString "Distance:" *> matchWhiteSpace *> splitOn matchWhiteSpace matchInt

parse :: Parser [(Int, Int)]
parse = Parser f
  where
    f inp = do
      (rest, times) <- runParser (parseTimes <* matchLine) inp
      (rest', dist) <- runParser parseDistances rest
      Right (rest', zip times dist)

countPossibleWins :: (Int, Int) -> Int
countPossibleWins (time, dist) = length $ filter (dist <) $ map getDist [0 .. time]
  where
    getDist t = (time - t) * t

pt1 :: String -> Either String Int
pt1 inp = do
  (_, races) <- runParser parse inp
  Right $ product $ map countPossibleWins races

parseTime :: Parser Int
parseTime = read . concatMap show <$> (matchString "Time:" *> matchWhiteSpace *> splitOn matchWhiteSpace matchInt)

parseDistance :: Parser Int
parseDistance = read . concatMap show <$> (matchString "Distance:" *> matchWhiteSpace *> splitOn matchWhiteSpace matchInt)

pt2 :: String -> Either String Int
pt2 inp = do
  (rest, time) <- runParser (parseTime <* matchLine) inp
  (_, distance) <- runParser parseDistance rest
  Right $ countPossibleWins (time, distance)
