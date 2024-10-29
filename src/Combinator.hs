module Combinator where

import Control.Applicative (Alternative (..))

data Term = Expression [Term] | Element String | Abstraction Term String
  deriving (Eq)

instance Show Term where
  show (Element e) = e
  show (Expression es) = "(" ++ formatExprs es ++ ")"
  show (Abstraction e v) = "[" ++ show e ++ "]_" ++ v

formatExprs :: [Term] -> String
formatExprs = foldl (\acc e -> acc ++ addSpace acc ++ show e) ""
  where
    addSpace acc = if null acc then "" else " "

reduceParensT :: Term -> Term
reduceParensT (Expression [t]) =
  removeSingleNestedParens $
    removeFirstElemParens $ reduceParensT t
reduceParensT (Expression ts) =
  removeSingleNestedParens $
    removeFirstElemParens $
      Expression (map reduceParensT ts)
reduceParensT (Abstraction (Element s) v) = Abstraction (Element s) v
reduceParensT (Abstraction (Expression [t]) v) = Abstraction t v
reduceParensT (Abstraction (Expression ts) v) = Abstraction (reduceParensT $ Expression ts) v
reduceParensT t = t

reduceParensC :: Combinator -> Combinator
reduceParensC = cMutMap reduceParensT

removeFirstElemParens :: Term -> Term
removeFirstElemParens (Expression (t : ts)) = case t of
  Expression ts' -> Expression (ts' ++ ts)
  _ -> Expression (t : ts)
removeFirstElemParens t = t

removeSingleNestedParens :: Term -> Term
removeSingleNestedParens (Expression ts) = Expression (map f ts)
  where
    f (Expression [t]) = t
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
  show (Pure name expr) = name ++ " = " ++ show expr
  show (Impure name args expr) = name ++ formattedArgs ++ " = " ++ show expr
    where
      formattedArgs = concatMap (" " ++) args

addAbstraction :: Combinator -> Maybe Combinator
addAbstraction (Pure _ _) = Nothing
addAbstraction (Impure _ [] _) = Nothing
addAbstraction (Impure n as t) = Just $ reduceParensC $ Impure n (init as) (Abstraction t $ last as)

containsAbstractionT :: Term -> Bool
containsAbstractionT (Abstraction _ _) = True
containsAbstractionT (Expression ts) = any containsAbstractionT ts
containsAbstractionT _ = False

containsAbstractionC :: Combinator -> Bool
containsAbstractionC = cMap containsAbstractionT

containsT :: Term -> Term -> Bool
containsT t1 t2 = (t1 == t2) || f t2
  where
    f (Expression ts) = any (containsT t1) ts
    f (Abstraction t' _) = containsT t1 t'
    f (Element _) = False

containsC :: Term -> Combinator -> Bool
containsC t = cMap $ containsT t

abstractionSubstitutionT :: Term -> Term
abstractionSubstitutionT (Abstraction (Element s) v) =
  if containsT (Element s) (Element v)
    then -- [x]_x = I
      Element "I"
    else -- [f]_x = K f
    -- if f does not contain x
      Expression [Element "K", Element s]
abstractionSubstitutionT (Abstraction (Expression ts) v) =
  case ( containsT (Element v) $ Expression (init ts),
         containsT (Element v) $ last ts
       ) of
    (True, True) ->
      -- [fg]_x = S [f]_x [g]_x
      -- if both f and g contain x
      Expression
        [ Element "S",
          Abstraction (Expression $ init ts) v,
          Abstraction (last ts) v
        ]
    (False, True) -> case last ts of
      Element _ ->
        -- [fx]_x = f 
        -- if f does not contain x
        Expression (init ts)
      _ ->
        -- [fg]_x = B f [g]_x
        -- if f does not contain x but g does
        Expression [
          Element "B",
          Expression (init ts),
          Abstraction (last ts) v
        ]
    (True, False) ->
      -- [fg]_x = C [f]_x g 
      -- if f contains x but g does not
      Expression [
        Element "C",
        Abstraction (Expression $ init ts) v,
        last ts
      ]
    (False, False) ->
      -- [f]_x = K f
      -- if f does not contain x
      Expression
        [ Element "K",
          Expression ts
        ]
abstractionSubstitutionT (Expression ts) = Expression $ map abstractionSubstitutionT ts
abstractionSubstitutionT t = t

abstractionSubstitutionC :: Combinator -> Combinator
abstractionSubstitutionC = cMutMap abstractionSubstitutionT

manyAbsSubsT :: Term -> Term
manyAbsSubsT t = 
  if containsAbstractionT t
  then manyAbsSubsT (abstractionSubstitutionT $ reduceParensT t)
  else reduceParensT t

manyAbsSubsC :: Combinator -> Combinator
manyAbsSubsC = cMutMap manyAbsSubsT

abstractionElimination :: Combinator -> Maybe Combinator
abstractionElimination c = 
  (addAbstraction c >>= (abstractionElimination . manyAbsSubsC)) <|> Just c

cMap :: (Term -> a) -> Combinator -> a
cMap f (Pure _ t) = f t
cMap f (Impure _ _ t) = f t

cMutMap :: (Term -> Term) -> Combinator -> Combinator
cMutMap f (Pure n t) = Pure n $ f t
cMutMap f (Impure n as t) = Impure n as $ f t
