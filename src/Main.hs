import Context
import Data.Foldable (traverse_)
import Parse
import Control.Monad

files :: [String]
files = ["prelude.ski", "nums.ski"]

main :: IO ()
main = do
  x <- foldl (\acc file -> acc >>= ((join <$>) . traverse (`parseFile` file)) ) (return $ Just emptyContext) files
  traverse_ print x


