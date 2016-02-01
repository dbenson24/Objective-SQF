module Main where

import Parser

import qualified Emit 

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> do
                    print $ Emit.codegenTop ex
                    print ex

main :: IO ()
main = do
    getContents >>= process
