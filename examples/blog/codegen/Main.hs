module Main (main) where

import BlogSchema (blogSchema)
import Poppy.Codegen.CLI (generate)

main :: IO ()
main = generate "src/Schema" blogSchema
