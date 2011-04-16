{-# LANGUAGE DeriveDataTypeable #-}
module LoopUnrolling.Annotations (
  Peel(..)
, Unroll(..)
) where
import Data.Data

data Peel = Peel Int deriving (Data, Typeable)

data Unroll = Unroll Int deriving (Data, Typeable)
