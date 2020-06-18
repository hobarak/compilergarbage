{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Kaleidoscope.Runner where

import Control.Monad (void)
import Control.Monad.Trans
import Kaleidoscope.AST
import Kaleidoscope.Codegen (codegen, emptyModule)
import qualified LLVM.AST as AST
import System.Console.Haskeline
import System.Environment
import System.IO

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Just modn -> loop modn
            Nothing -> loop mod

runner :: IO ()
runner = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> void (processFile fname)
    _ -> error "asd"

runner2 fname = void (processFile fname)