{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Redundant id" #-}
module Day2 (pt1, pt2) where

import AocParse
import Control.Applicative
import Day2.Types
import Prelude hiding (id)

--- PARSE ---
parseRed :: Parser Cube
parseRed = Red <$> (matchInt <* matchWhiteSpace <* matchString "red")

parseGreen :: Parser Cube
parseGreen = Green <$> (matchInt <* matchWhiteSpace <* matchString "green")

parseBlue :: Parser Cube
parseBlue = Blue <$> (matchInt <* matchWhiteSpace <* matchString "blue")

parseColour :: Parser Cube
parseColour = parseRed <|> parseGreen <|> parseBlue

parseHandful :: Parser [Cube]
parseHandful = splitOn (matchString ", ") parseColour

parseGame :: Parser [Handful]
parseGame = map foldHandful <$> splitOn (matchString "; ") parseHandful

parseId :: Parser Int
parseId = matchString "Game " *> matchInt <* matchString ": "

parseLine :: Parser Game
parseLine = Parser f
  where
    f inp = do
      (game, i) <- runParser parseId inp
      (rest, hs) <- runParser parseGame game
      Right (rest, Game i hs)

parse :: Parser [Game]
parse = splitOn (matchChar '\n') parseLine

--- CALC ---
foldHandful :: [Cube] -> Handful
foldHandful = foldl f (Handful 0 0 0)
  where
    f h (Red n) = Handful (red h + n) (green h) (blue h)
    f h (Green n) = Handful (red h) (green h + n) (blue h)
    f h (Blue n) = Handful (red h) (green h) (blue h + n)

maxOfHandful :: Handful -> Handful -> Handful
maxOfHandful a b = Handful (red a `max` red b) (green a `max` green b) (blue a `max` blue b)

maxOfHandfuls :: [Handful] -> Handful
maxOfHandfuls = foldl maxOfHandful (Handful 0 0 0)

gamePossible :: Handful -> Game -> Bool
gamePossible a (Game _ hs) =
  red a >= red b
    && green a >= green b
    && blue a >= blue b
  where
    b = maxOfHandfuls hs

getPower :: Game -> Int
getPower (Game _ hs) = red m * green m * blue m
  where
    m = maxOfHandfuls hs

--- RUN ---
pt1 :: String -> Either String Int
pt1 inp = do
  (_, parsed) <- runParser parse inp
  Right $ sum $ map id $ filter (gamePossible (Handful 12 13 14)) parsed

pt2 :: String -> Either String Int
pt2 inp = do
  (_, parsed) <- runParser parse inp
  Right $ sum $ map getPower parsed
