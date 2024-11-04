module Context where

import Combinator
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

newtype Context = Context [Combinator]
  deriving (Show, Eq)


registerAll = undefined
