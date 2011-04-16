module LoopUnrolling.Plugin where

import LoopUnrolling.Pass

import GHCPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _option todos = do
    -- Must simplify before hand to get accurate correct recursive loops
    return $ defaultGentleSimplToDo : CoreDoPluginPass "Peel and unroll loops" (BindsToBindsPluginPass peelUnrollLoopsProgram) : todos
