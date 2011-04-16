module Main where

import LoopUnrolling.Annotations

main = do
    print $ accumulator_odd 10

accumulator_odd, accumulator_even :: Int -> [Bool]

{-# ANN accumulator_odd (Unroll 2) #-}
accumulator_odd n
  | n < 0     = []
  | otherwise = True : accumulator_even (n - 1)

{-# ANN accumulator_even (Unroll 4) #-}
accumulator_even n
  | n < 0     = []
  | otherwise = False : accumulator_odd (n - 1)


-- We should get some beautiful inner loops like this:
--
-- Rec {
-- $waccumulator_odd_s1QT [ALWAYS LoopBreaker Nothing] :: GHC.Prim.Int#
--                                                        -> [GHC.Bool.Bool]
-- LclId
-- [Arity 1
--  Str: DmdType L]
-- $waccumulator_odd_s1QT =
--   \ (ww_s1QQ :: GHC.Prim.Int#) ->
--     case GHC.Prim.<# ww_s1QQ 0 of _ {
--       GHC.Bool.False ->
--         GHC.Types.:
--           @ GHC.Bool.Bool
--           GHC.Bool.True
--           (let {
--              x_X1nJ [ALWAYS Just L] :: GHC.Prim.Int#
--              LclId
--              [Str: DmdType]
--              x_X1nJ = GHC.Prim.-# ww_s1QQ 1 } in
--            case GHC.Prim.<# x_X1nJ 0 of _ {
--              GHC.Bool.False ->
--                GHC.Types.:
--                  @ GHC.Bool.Bool
--                  GHC.Bool.False
--                  (let {
--                     x_X1nR [ALWAYS Just L] :: GHC.Prim.Int#
--                     LclId
--                     [Str: DmdType]
--                     x_X1nR = GHC.Prim.-# x_X1nJ 1 } in
--                   case GHC.Prim.<# x_X1nR 0 of _ {
--                     GHC.Bool.False ->
--                       GHC.Types.:
--                         @ GHC.Bool.Bool
--                         GHC.Bool.True
--                         (let {
--                            x_X1nZ [ALWAYS Just L] :: GHC.Prim.Int#
--                            LclId
--                            [Str: DmdType]
--                            x_X1nZ = GHC.Prim.-# x_X1nR 1 } in
--                          case GHC.Prim.<# x_X1nZ 0 of _ {
--                            GHC.Bool.False ->
--                              GHC.Types.:
--                                @ GHC.Bool.Bool
--                                GHC.Bool.False
--                                ($waccumulator_odd_s1QT (GHC.Prim.-# x_X1nZ 1));
--                            GHC.Bool.True -> GHC.Types.[] @ GHC.Bool.Bool
--                          });
--                     GHC.Bool.True -> GHC.Types.[] @ GHC.Bool.Bool
--                   });
--              GHC.Bool.True -> GHC.Types.[] @ GHC.Bool.Bool
--            });
--       GHC.Bool.True -> GHC.Types.[] @ GHC.Bool.Bool
--     }
-- end Rec }

-- Unfortunately, they don't constant fold because GHC isn't smart enough to float
-- unlifted things :-)