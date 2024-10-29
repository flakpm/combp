{-# LANGUAGE InstanceSigs #-}

module Parse (parseExp, parseCombinator) where

import Combinator (Combinator (..), Term (..))
import Control.Applicative (Alternative (..))
import Data.Char (isAlphaNum, isSpace)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', x) <- p2 input'
      Just (input'', f x)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = ifP (c ==)

ifP :: (Char -> Bool) -> Parser Char
ifP f = Parser p
  where
    p (x : xs)
      | f x = Just (xs, x)
      | otherwise = Nothing
    p [] = Nothing

delimitedP :: Parser a -> Parser b -> Parser c -> Parser b
delimitedP p1 p2 p3 = p1 *> p2 <* p3

singleCharElemP :: Parser Term
singleCharElemP = Element . return <$> ifP isAlphaNum

delimitedElemP :: Parser Term
delimitedElemP = Element <$> delimitedP (charP '\\') (some $ ifP isAlphaNum) (charP '\\')

elemP :: Parser Term
elemP = singleCharElemP <|> delimitedElemP

subexpP :: Parser Term
subexpP = delimitedP (charP '(') expressionP (charP ')')

expressionP :: Parser Term
expressionP = Expression <$> some (subexpP <|> elemP)

parseExp :: String -> Maybe Term
parseExp = runTopLevelParser expressionP

pureCombP :: Parser Combinator
pureCombP = Parser $ \input -> do
  (input', name) <- runParser elemP input
  (input'', _) <- runParser (charP '=') input'
  (input''', expression) <- runParser expressionP input''
  return (input''', Pure (show name) expression)

impureCombP :: Parser Combinator
impureCombP = Parser $ \input -> do
  (input', name) <- runParser elemP input
  (input'', args) <- runParser (many elemP) input'
  (input''', _) <- runParser (charP '=') input''
  (input'''', expression) <- runParser expressionP input'''
  return (input'''', Impure (show name) (map show args) expression)

combP :: Parser Combinator
combP = pureCombP <|> impureCombP

parseCombinator :: String -> Maybe Combinator
parseCombinator = runTopLevelParser combP

runTopLevelParser :: Parser a -> String -> Maybe a
runTopLevelParser p s = snd <$> runParser (allConsumingP p) (filter (not . isSpace) s)

allConsumingP :: Parser a -> Parser a
allConsumingP (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  if null input' then Just (input', x) else Nothing
