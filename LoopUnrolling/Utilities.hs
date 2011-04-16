module LoopUnrolling.Utilities where

import GHCPlugins

import Data.Data
import Data.Maybe


annotationsOn :: Data a => CoreBndr -> CoreM [a]
annotationsOn b = findAnnotations deserializeWithData (NamedTarget $ varName b)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

mkCloneId :: CoreBndr -> CoreM CoreBndr
mkCloneId b = mkUserLocalM (nameOccName $ idName b) (idType b) (getSrcSpan (idName b))