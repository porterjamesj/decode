module Main where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer

data Tok =
  TBlock Int [Tok] |
  TChar Char

instance Show Tok where
  show (TChar c) = [c]
  show (TBlock i toks) = join $ replicate i s
    where s = concatMap show toks

int :: Parser Int
int = fromIntegral <$> integer

parseChar :: Parser Tok
parseChar = TChar <$> lowerChar

parseBlock :: Parser Tok
parseBlock = TBlock <$> int <*> between (char '[') (char ']') (many parseTok)

parseTok :: Parser Tok
parseTok = parseBlock <|> parseChar

parseInput :: Parser [Tok]
parseInput = many parseTok <* eof

main :: IO ()
main = do
  i <- getLine
  let result = parse parseInput "stdin" i
  putStr $ either parseErrorPretty (concatMap show) result
