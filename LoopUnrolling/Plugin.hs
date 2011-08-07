module LoopUnrolling.Plugin where
import LoopUnrolling.Pass (peelUnrollLoopsProgram)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

-- Must simplify before hand to get accurate correct recursive loops
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do 
    reinitializeGlobals
    return $ gentle_simpl : unroll : todos
  where unroll = CoreDoPluginPass "Peel/unroll loops" peelUnrollLoopsProgram
        gentle_simpl = CoreDoSimplify 10 $ SimplMode { sm_phase = InitialPhase
                                                     , sm_names = ["Gentle"]
                                                     , sm_rules = False
                                                     , sm_inline = False
                                                     , sm_case_case = False
                                                     , sm_eta_expand = False }
