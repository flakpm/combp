module Context where

import Combinator
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

newtype Context = Context [Combinator]

register :: Context -> Combinator -> Maybe Context
register (Context cs) c = if validateC c then Just $ Context (c : cs) else Nothing

validateT :: Term -> Bool
validateT (Element s) = s `elem` ["S", "K", "I", "B", "C"]
validateT (Expression ts) = validateT $ Expression ts
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
