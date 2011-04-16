module LoopUnrolling.Utilities where

import GHCPlugins

import Data.Data
import Data.Maybe


annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] (varUnique bndr)

orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

mkCloneId :: CoreBndr -> CoreM CoreBndr
mkCloneId b = mkUserLocalM (nameOccName $ idName b) (idType b) (getSrcSpan (idName b))