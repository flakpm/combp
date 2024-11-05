module Context where

import Combinator
import Data.List

newtype Context = Context [Combinator]
  deriving (Show, Eq)

register :: Context -> Combinator -> Maybe Context
register (Context cs) c = if all ((combinatorName c /=) . combinatorName) cs then Just $ Context (argElim c:cs) else Nothing

lazyReduce :: CombExp c => Context -> c -> c
lazyReduce (Context cs) = ctransform lazyReduce'
  where
    lazyReduce' expr = last $ expr:unfoldr (fmap (\x -> (x, x)) . lazyReduceOnce) expr
    lazyReduceOnce (Expression (Element "S":x:y:z:ts)) = Just $ Expression $ [x, z, Expression [y, z]] ++ ts
    lazyReduceOnce (Expression (Element "K":x:_:ts)) = Just $ Expression $ x:ts
    lazyReduceOnce (Expression (Element "I":x:ts)) = Just $ Expression $ x:ts
    lazyReduceOnce (Expression (Element "B":x:y:z:ts)) = Just $ Expression $ [x, Expression [y, z]] ++ ts
    lazyReduceOnce (Expression (Element "C":x:y:z:ts)) = Just $ Expression $ [x, z, y] ++ ts
    lazyReduceOnce (Expression (Element s:ts)) =
      (\c -> Expression $ combinatorExpr c:ts) <$> find (\x -> s == combinatorName x) cs
    lazyReduceOnce (Expression (Expression ts':ts)) = Just $ Expression $ ts' ++ ts
    lazyReduceOnce _ = Nothing

eagerReduce :: CombExp c => Context -> c -> c
eagerReduce c = ctransform eagerReduce'
  where
    eagerReduce' = recurse . lazyReduce c
    recurse (Expression ts) = Expression $ map (\t -> case eagerReduce c t of
      (Expression [ts']) -> ts'
      t' -> t') ts
    recurse t = t

emptyContext :: Context
emptyContext = Context []
