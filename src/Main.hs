import Data.Foldable (traverse_)
import Parse
import Combinator
import Context

main :: IO ()
main = do
  basis <- readFile "basis.ski"
  let combinators = traverse parseComb $ lines basis
  let string = unlines . map show <$> combinators
  traverse_ putStrLn string
