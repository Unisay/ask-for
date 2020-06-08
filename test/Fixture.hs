{-# LANGUAGE DeriveGeneric #-}

module Fixture where

import Relude

data Foo = Foo1 | Foo2 | Foo3
  deriving (Show)

data FooWrapper = FooWrapper Foo
  deriving (Generic, Show)

data Config = Config
  { tup :: (SubConfig, Word),
    sub :: Maybe SubConfig,
    tak :: Char
  }
  deriving (Generic, Show)

data SubConfig = SubConfig
  { fow :: FooWrapper,
    boo :: Bool,
    ioo :: Int
  }
  deriving (Generic, Show)

testConfig :: Config
testConfig =
  Config
    { tup = (subConfig, 43),
      sub = Nothing,
      tak = 't'
    }

subConfig :: SubConfig
subConfig =
  SubConfig
    { fow = FooWrapper Foo1,
      boo = True,
      ioo = 42
    }

nestedTuples :: (FooWrapper, Int)
nestedTuples = (FooWrapper Foo2, 42)
