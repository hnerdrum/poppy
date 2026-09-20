module Main (main) where

import Poppy.Codegen.CLI (generate)
import TaskSchema (taskSchema)

main :: IO ()
main = generate "src/Schema" taskSchema
