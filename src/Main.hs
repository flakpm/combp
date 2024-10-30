import Combinator
import Context
import Data.Foldable (traverse_)
import Parse

main :: IO ()
main = do
  prelude <- readFile "prelude.ski"
  let combinators = traverse parseComb $ lines prelude
  let context = combinators >>= registerAll
  traverse_ print context

registerAll :: [Combinator] -> Maybe Context
registerAll = foldl f (Just $ Context [])

f :: Maybe Context -> Combinator -> Maybe Context
f acc x = do
  con <- acc
  y <- makePure $ contextSubsC con $ abstractionElimination x
  register con y
