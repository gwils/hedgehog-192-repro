module Main where

import Shared
import Data.IORef

main :: IO ()
main = do
  r <- newIORef (0 :: Int)
  hedgehogDirectly r
