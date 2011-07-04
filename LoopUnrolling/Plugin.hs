module LoopUnrolling.Plugin where
import LoopUnrolling.Pass (peelUnrollLoopsProgram)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

-- Must simplify before hand to get accurate correct recursive loops
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = return $ defaultGentleSimplToDo : unroll : todos
  where unroll = CoreDoPluginPass "Peel/unroll loops" peelUnrollLoopsProgram
