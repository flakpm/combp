module Combinator where

data Term = Expression [Term] | Element String | Abstraction Term String
  deriving (Eq)

instance Show Term where
  show (Element e) = e
  show (Abstraction e v) = "[" ++ show e ++ "]_" ++ v
  show (Expression es) = "(" ++ formatExprs es ++ ")"
    where
      formatExprs = foldl (\acc e -> acc ++ addSpace acc ++ show e) ""
      addSpace acc = if null acc then "" else " "

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
  show (Pure name expr) = name ++ " = " ++ show expr
  show (Impure name args expr) = name ++ formattedArgs ++ " = " ++ show expr
    where
      formattedArgs = concatMap (" " ++) args

class CombExp c where 
  cmap :: (Term -> a) -> c -> a
  ctransform :: (Term -> Term) -> c -> c

instance CombExp Term where 
  cmap = id
  ctransform = id

instance CombExp Combinator where 
  cmap f (Pure _ ts) = f ts 
  cmap f (Impure _ _ ts) = f ts
  ctransform f (Pure n ts) = Pure n $ f ts
  ctransform f (Impure n as ts) = Impure n as $ f ts

contains :: CombExp c => Term -> c -> Bool
contains t = cmap contains'
  where 
    contains' (Element s)        = t == (Element s)
    contains' (Expression ts)    = any (contains t) ts || t == (Expression ts)
    contains' (Abstraction ts _) = contains t ts

containsAbst :: CombExp c => c -> Bool
containsAbst = cmap containsAbst'
  where 
    containsAbst' (Abstraction _ _) = True
    containsAbst' (Expression ts)   = any containsAbst ts
    containsAbst' (Element _)       = False



reduceParensT = undefined
reduceParensC = undefined
contextSubsT = undefined
reduceAllT = undefined


