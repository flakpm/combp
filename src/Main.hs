import Parse

main :: IO ()
main = do
  prelude <- readFile "prelude.ski"
  print $ traverse parseCombinator $ lines prelude
