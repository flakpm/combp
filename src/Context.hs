module Context where

import Combinator
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

newtype Context = Context [Combinator]
  deriving (Show, Eq)

register :: Context -> Combinator -> Maybe Context
register (Context cs) c = if validateC c then Just $ Context (c : cs) else Nothing

validateT :: Term -> Bool
validateT (Element s) = s `elem` ["S", "K", "I", "B", "C"]
validateT (Expression ts) = all validateT ts
validateT (Abstraction _ _) = False

validateC :: Combinator -> Bool
validateC (Pure n ts) = cMap validateT $ Pure n ts
validateC (Impure {}) = False

contextSubsT :: Context -> Term -> Term
contextSubsT (Context cs) (Element s) = fromMaybe (Element s) $ foldl f Nothing cs
  where
    f mt (Pure n ts) = mt <|> (if s == n then Just ts else Nothing)
    f _ _ = Nothing
contextSubsT c (Expression ts) = Expression $ map (contextSubsT c) ts
contextSubsT _ t = t

contextSubsC :: Context -> Combinator -> Combinator
contextSubsC c = cMutMap (contextSubsT c)

reduceOnceT :: Term -> Term
reduceOnceT (Expression e) = reduceParensT $ Expression $ map reduceOnceT $ f e
  where
    f :: [Term] -> [Term]
    f ts = case ts of
      ((Element s) : x : y : z : ts') | s == "S" -> x : z : Expression [y, z] : ts'
      ((Element k) : x : _ : ts') | k == "K" -> x : ts'
      ((Element i) : x : ts') | i == "I" -> x : ts'
      ((Element b) : x : y : z : ts') | b == "B" -> x : Expression [y, z] : ts'
      ((Element c) : x : y : z : ts') | c == "C" -> x : z : y : ts'
      _ -> ts
reduceOnceT t = t

reduceOnceC :: Combinator -> Combinator
reduceOnceC = cMutMap reduceOnceT

reduceAllT :: Term -> Term
reduceAllT t =
  if reduceOnceT t == t
    then t
    else reduceAllT $ reduceOnceT t

reduceAllC :: Combinator -> Combinator
reduceAllC = cMutMap reduceAllT
