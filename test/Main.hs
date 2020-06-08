{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Ask
import Fixture
import Relude

main :: IO ()
main = do
  print $ Ask.for @Foo nestedTuples
  print $ Ask.for @Foo subConfig
  print $ Ask.for @Foo testConfig
  exitSuccess
