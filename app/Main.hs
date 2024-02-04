module Main where

import qualified Server (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Server.someFunc
