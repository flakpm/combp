import Combinator
import Context
import Data.Foldable (traverse_)
import Parse

main :: IO ()
main = do
  basis <- readFile "basis.ski"
  let combinators = traverse parseComb $ lines basis
  let string = unlines . map show <$> combinators
  traverse_ putStrLn string
