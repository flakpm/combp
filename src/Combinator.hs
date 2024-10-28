{-# LANGUAGE InstanceSigs #-}

module Combinator where

data Term = SubExpression [Term] | Element String | BracketAbstraction [Term] String
  deriving (Eq)

instance Show Term where
  show :: Term -> String
  show (Element e) = e
  show (SubExpression es) = "(" ++ formatExprs es ++ ")"
  show (BracketAbstraction es v) = "[" ++ formatExprs es ++ "]_" ++ v

formatExprs :: [Term] -> String
formatExprs = foldl (\acc e -> acc ++ addSpace acc ++ show e) ""
  where
    addSpace acc = if null acc then "" else " "

reduceParens :: Term -> Term
reduceParens (SubExpression es) =
  removeSingleNestedParens $
    removeFirstElemParens $
      SubExpression (map reduceParens es)
reduceParens t = t

removeFirstElemParens :: Term -> Term
removeFirstElemParens (SubExpression (e : es)) = case e of
  SubExpression es' -> SubExpression (es' ++ es)
  _ -> SubExpression (e : es)
removeFirstElemParens t = t

removeSingleNestedParens :: Term -> Term
removeSingleNestedParens (SubExpression es) = SubExpression (map f es)
  where
    f (SubExpression [e]) = e
    f t = t
removeSingleNestedParens t = t

data Combinator
  = Pure
      { combinatorName :: String,
        combinatorExpr :: Term
      }
  | Impure
      { combinatorName :: String,
        combinatorArgs :: [String],
        combinatorExpr :: Term
      }
  deriving (Eq)

instance Show Combinator where
  show :: Combinator -> String
  show (Pure name expr) = name ++ " = " ++ show expr
  show (Impure name args expr) = name ++ formattedArgs ++ " = " ++ show expr
    where
      formattedArgs = concatMap (" " ++) args
