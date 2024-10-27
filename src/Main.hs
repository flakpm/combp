{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (Alternative (..))
import Data.Char (isAlphaNum, isSpace)

main :: IO ()
main = print "Hello from combp!"

data Expression = Expression [Expression] | Element String
  deriving (Eq)

instance Show Expression where
  show :: Expression -> String
  show (Element e) = e
  show (Expression es) = "(" ++ formatted ++ ")"
    where
      formatted = foldl (\acc e -> acc ++ addSpace acc ++ show e) "" es
      addSpace acc = if null acc then "" else " "

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
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = ifP (c ==)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

ifP :: (Char -> Bool) -> Parser Char
ifP f = Parser p
  where
    p (x : xs)
      | f x = Just (xs, x)
      | otherwise = Nothing
    p [] = Nothing

delimitedP :: Parser a -> Parser b -> Parser c -> Parser b
delimitedP p1 p2 p3 = p1 *> p2 <* p3

singleCharElemP :: Parser Expression
singleCharElemP = Element . return <$> ifP isAlphaNum

delimitedElemP :: Parser Expression
delimitedElemP = Element <$> delimitedP (charP '\\') (many1 $ ifP isAlphaNum) (charP '\\')

elemP :: Parser Expression
elemP = singleCharElemP <|> delimitedElemP

subexpP :: Parser Expression
subexpP = delimitedP (charP '(') expressionP (charP ')')

expressionP :: Parser Expression
expressionP = Expression <$> many1 (subexpP <|> elemP)

allExpP :: Parser Expression
allExpP = Parser (\input -> Just (filter (not . isSpace) input, ())) *> allConsumingP expressionP

allConsumingP :: Parser a -> Parser a
allConsumingP (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  if null input' then Just (input', x) else Nothing
