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
    contains' (Element s)        = t == Element s
    contains' (Expression ts)    = any (contains t) ts || t == Expression ts
    contains' (Abstraction ts _) = contains t ts

containsAbst :: CombExp c => c -> Bool
containsAbst = cmap containsAbst'
  where
    containsAbst' (Abstraction _ _) = True
    containsAbst' (Expression ts)   = any containsAbst ts
    containsAbst' (Element _)       = False

abstract :: Combinator -> Combinator
abstract (Impure n [a] ts) = Pure n $ Abstraction ts a
abstract (Impure n as ts) = Impure n (init as) $ Abstraction ts $ last as
abstract c = c

abstSubs :: CombExp c => c -> c
abstSubs = ctransform abstSubs'
  where
    abstSubs' (Abstraction t v) = case t of
      (Element s) | s == v -> Element "I"
      (Element s) | s /= v -> Expression [Element "K", Element s]
      (Expression ts) -> case ts of
        [t'] | contains (Element v) t' -> abstSubs $ Abstraction t' v
        [t'] | not (contains (Element v) t') -> Expression [Element "K", t']
        ts' | contains (Element v) (Expression $ init ts')
            && contains (Element v) (last ts')
            -> Expression [Element "S", abstSubs $ Abstraction (Expression $ init ts') v, abstSubs $ Abstraction (last ts') v]
        ts' | not (contains (Element v) (Expression $ init ts'))
            && contains (Element v) (last ts')
            -> if last ts' == Element v then Expression $ init ts'
              else Expression [Element "B", Expression $ init ts', abstSubs $ Abstraction (last ts') v]
        ts' | contains (Element v) (Expression $ init ts')
            && not (contains (Element v) (last ts'))
            -> Expression [Element "C", abstSubs $ Abstraction (Expression $ init ts') v , last ts']
        ts' | not (contains (Element v) (Expression $ init ts'))
            && not (contains (Element v) (last ts'))
            -> Expression [Element "K", Expression ts']
        _ -> error "This should not happen."
      t' -> t'
    abstSubs' (Expression ts) = Expression $ map abstSubs ts
    abstSubs' t = t

argElim :: Combinator -> Combinator
argElim (Impure n [a] t) = abstSubs $ abstract $ Impure n [a] t
argElim (Impure n as t) = argElim $ abstSubs $ abstract $ Impure n as t
argElim c = c

