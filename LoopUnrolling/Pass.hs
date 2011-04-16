module LoopUnrolling.Pass (peelUnrollLoopsProgram) where

import GHCPlugins

import LoopUnrolling.Plugin.Annotations
import LoopUnrolling.Plugin.Utilities

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List


peelUnrollLoopsProgram :: [CoreBind] -> CoreM [CoreBind]
peelUnrollLoopsProgram = mapM peelUnrollBind

peelUnrollBind :: CoreBind -> CoreM CoreBind
peelUnrollBind (NonRec b e) = return $ NonRec b e
peelUnrollBind (Rec bes) = do
    let bs = map fst bes
    peel_amnt   <- forM bs $ \b -> annotationsOn b >>= (return . flattenPeelAnns)
    unroll_amnt <- forM bs $ \b -> annotationsOn b >>= (return . flattenUnrollAnns)
    
    let -- When PEELing, tie the first replication back to itself so the others can get inlined
        tieback_peel bs = fromJust $ head bs
        -- When UNROLLing, tie the first replication back to the last so we can inline everything into the first
        tieback_unroll bs = last [b | Just b <- bs]
    
    (bes', peeled_bes)    <- replicateBindGroup peel_amnt   bes  tieback_peel
    (bes'', unrolled_bes) <- replicateBindGroup unroll_amnt bes' tieback_unroll 
    return $ Rec $ bes'' ++ unrolled_bes ++ peeled_bes

flattenPeelAnns :: [Peel] -> Maybe Int
flattenPeelAnns [] = Nothing
flattenPeelAnns ps = Just $ maximum [n | Peel n <- ps] -- Peel something up to the maximum number of times annotated

flattenUnrollAnns :: [Unroll] -> Maybe Int
flattenUnrollAnns [] = Nothing
flattenUnrollAnns us = Just $ maximum [n | Unroll n <- us] -- Unroll something up to the maximum number of times annotated


replicateBindGroup :: [Maybe Int]                    -- ^ Number of times to replicate each binding
                   -> [(CoreBndr, CoreExpr)]         -- ^ Bindings
                   -> ([Maybe CoreBndr] -> CoreBndr) -- ^ Given a list of all iterations of a binding, choose the one to tie back to
                   -> CoreM ([(CoreBndr, CoreExpr)], [(CoreBndr, CoreExpr)])
replicateBindGroup replicate_amnt orig_bes tieback_strategy = do
    let (orig_bs, orig_es) = unzip orig_bes
    
    -- We have to run the replication loop as many times as the maximum PEEL/UNROLL annotation claims.
    -- Find out what that number is:
    let maximum_peel = 1 + maximum (0 : [n | Just n <- replicate_amnt])
      -- NB: we increase the number by 1 because we're going to use the first ``peeling''/``unrolling'' just to copy the function body
    
    -- Generate the final names we want to bind everything to. Imagine we were peeling/unrolling f n times,
    -- g m times and h not at all. Then we want to bind each peeling/unrolling to names like so:
    --
    -- Fn. / Itr.   0     1    ... n   n+1   ... m   m+1
    -- f            f_0   f_1  ... f   f     ... f   f
    -- g            g_0   g_1  ... g_n g_n+1 ... g   g
    -- h            h     h    ... h   h     ... h   h
    --
    -- NB: this is a slight divergence from the spec, which claims that we should peel/unroll f up to m
    -- iterations just because g is being peeled/unrolled that much.  If that's true, why aren't we peeling
    -- h that much as well?
    all_bs <- forM (orig_bs `zip` replicate_amnt) $ \(b, mb_p) -> do
                        let p = mb_p `orElse` 0
                        new_bs <- replicateM p (mkCloneId b)
                        
                        -- Just means ``replicate'', Nothing means ``don't replicate, just refer to the previous replication''.
                        -- INVARIANT: we always replicate at least once.
                        -- INVARIANT: we always have a string of Just followed by a string of Nothing.
                        return $ map Just new_bs ++ take (maximum_peel - p) (Just b : repeat Nothing)
    
    let -- The above has a list of binders per BINDER. We want a list per ITERATION:
        all_bs_by_iter = transpose all_bs
        
        -- The first replication EITHER:
        --  1) Restores the functionality of the inner loop, when PEELing
        --  2) Is just a normal (but non-INLINE) iteration of the loop when UNROLLing, but
        --     which ties back to the last replication we will create
        --
        -- Here, we work out which to do.  This is done by, for each binder, picking one of the
        -- corresponding replicated binders as the tieback binder
        tieback_bs = map tieback_strategy all_bs
        
        -- Do the first replication
        (mb_first_iter_bs : rest_all_bs_by_iter) = all_bs_by_iter
        first_iter_bs = map fromJust mb_first_iter_bs
        first_iter_es = buildOneIteration orig_es (orig_bs `zip` tieback_bs)
        first_iter_binds = first_iter_bs `zip` first_iter_es
         -- NB: do NOT mkInlineMe here. Two reasons:
         --  1) In PEEL it's pointless, because this is a recursive loop and GHC won't inline anyway
         --  2) It might screw up UNROLL, because in order to get the unrollings inlined nicely
         --     we need at least one non-INLINEd thing in the group of unrolled definitions.
    
        -- We use this loop to actually do the business of replicating everything the remaining number of times:
        go []                               last_iter_bs = []
        go (mb_this_iter_bs : rest_iter_bs) last_iter_bs = extra_binds ++ rest_binds
          -- OK, replicate any expressions that still have more replications to go
          where
            (es_to_peel, this_iter_bs) = unzip [(e, b') | (e, Just b') <- orig_es `zip` mb_this_iter_bs]
            this_iter_es = buildOneIteration es_to_peel (orig_bs `zip` last_iter_bs)
            -- If we are replicating for any time other than the first (handled by the code above), then make
            -- the body look small by adding an __inline_me.
            -- The intention is not only to optimize, but to to prevent the compiler from messing
            -- with it (please see http://hackage.haskell.org/trac/ghc/wiki/Inlining for more).
            extra_binds = this_iter_bs `zip` map mkInlineMe this_iter_es
            
            -- If we produced a new binding for an e this iteration, we want to use it instead of the copy
            -- of the expression from the last invocation of go.  Otherwise, use the last generated one.
            rest_binds = go rest_iter_bs $ zipWith (flip fromMaybe) mb_this_iter_bs last_iter_bs
    
    -- Done! Put together the two sets of bindings
    putMsgS (showSDocDebug $ ppr all_bs_by_iter)
    return $ (first_iter_binds, go rest_all_bs_by_iter first_iter_bs)

buildOneIteration :: [CoreExpr] -> [(CoreBndr, CoreBndr)] -> [CoreExpr]
buildOneIteration es_to_peel subst_bs = map (substExpr subst) es_to_peel
  where
    -- Make a substitution mapping from the origin bs to the ones from our last iteration,
    -- then apply that to the original RHS to get a peeled version
    in_scope = mkInScopeSet $ exprsFreeVars es_to_peel
    subst = extendIdSubstList (mkEmptySubst in_scope) [(b, Var b') | (b, b') <- subst_bs]