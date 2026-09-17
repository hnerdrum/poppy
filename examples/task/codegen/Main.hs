module Main (main) where

import Poppy.Codegen.CLI (mainWith)
import TaskTarget (taskTarget)

main :: IO ()
main = mainWith [taskTarget] [taskTarget]
