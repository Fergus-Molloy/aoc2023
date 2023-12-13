{-# LANGUAGE GADTs #-}

module Day8 where

import AocParse
import Control.Applicative
import Day8.Helpers

data Instruction = L | R deriving stock (Show)

tinp :: String
tinp = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"

isEnd :: Node -> Bool
isEnd (Node n _) = n == "ZZZ"

parseNode :: Parser Node
parseNode = Parser f
  where
    f inp = do
      (rest, n) <- runParser (matchN 3) inp
      (rest', _) <- runParser (matchWhiteSpace *> matchString "=" <* matchWhiteSpace) rest
      (rest'', cs) <- runParser matchChildren rest'
      Right (rest'', Node n cs)

matchChildren :: Parser (String, String)
matchChildren = Parser f
  where
    f inp = do
      (rest, left) <- runParser (matchChar '(' *> matchN 3 <* matchString ", ") inp
      (rest', right) <- runParser (matchN 3 <* matchChar ')') rest
      Right (rest', (left, right))

parseInstructions :: Parser [Instruction]
parseInstructions = many ((L <$ matchChar 'L') <|> (R <$ matchChar 'R'))

parse :: Parser ([Instruction], [Node])
parse = Parser f
  where
    f inp = do
      (rest, is) <- runParser parseInstructions inp
      (rest', ns) <- runParser (matchString "\n\n" *> splitOn matchWhiteSpace parseNode) rest
      Right (rest', (is, ns))

pt1 :: String -> Either String Int
pt1 _ = Right 0

pt2 :: String -> Either String Int
pt2 _ = Right 0
