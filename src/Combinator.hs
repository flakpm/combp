{-# LANGUAGE InstanceSigs #-}

module Combinator where

data Expression = Expression [Expression] | Element String
  deriving (Eq)

instance Show Expression where
  show :: Expression -> String
  show (Element e) = e
  show (Expression es) = "(" ++ formattedExprs ++ ")"
    where
      formattedExprs = foldl (\acc e -> acc ++ addSpace acc ++ show e) "" es
      addSpace acc = if null acc then "" else " "

data Combinator = Pure String Expression | Impure String [String] Expression
  deriving (Eq)

instance Show Combinator where
  show :: Combinator -> String
  show (Pure name expr) = name ++ " = " ++ show expr
  show (Impure name args expr) = name ++ formattedArgs ++ " = " ++ show expr
    where
      formattedArgs = concatMap (" " ++) args
