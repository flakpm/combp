import Parse

main :: IO ()
main = do
  basis <- readFile "basis.ski"
  let combinators = traverse parseCombinator $ lines basis
  let string = unlines . map show <$> combinators
  _ <- sequenceA $ putStrLn <$> string
  return ()
