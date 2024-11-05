import Context

-- files :: [String]
-- files = ["prelude.ski", "nums.ski"]

-- main :: IO ()
-- main = do
--   context <- foldl accumContextIO (return $ Just emptyContext) files

--   putStrLn "Eval:"
--   input <- getLine

--   let e = context >>= (`evaluateT` input)

--   traverse_ print e

accumContextIO :: IO (Maybe Context) -> String -> IO (Maybe Context)
accumContextIO acc file = acc >>= ((join <$>) . traverse (`parseFile` file))

main :: IO ()
main = do 
  print "Hello World"
