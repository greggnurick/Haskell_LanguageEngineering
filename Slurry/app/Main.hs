-- This module defines a simple command line interface for the Slurry
-- system.  If your solution is correct, this module should just work.
module Main (main) where

import Ast
import Parser (parseString)

import Data
import Renderer (render)

import System.Environment(getArgs)

run :: Template -> Ctx -> IO ()
run t d =
  case render d t of
    Left e -> putStrLn ("*** Render error: " ++ show e)
    Right s -> putStr s

main :: IO ()
main = do args <- getArgs
          case args of
            ["-p", tfile] -> do
              ts <- readFile tfile
              case parseString ts of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> putStrLn $ show p
            ["-r", tfile, dfile] -> do
              ts <- readFile tfile
              ds <- readFile dfile
              let t = read ts
              let d = read ds
              run t d
            ["-b", tfile, dfile] -> do
              ts <- readFile tfile
              case parseString ts of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right t -> do
                  ds <- readFile dfile
                  let d = read ds
                  run t d
            _ -> putStrLn
                   "Use one of:\n\
                   \  slurry -p TEMPLATE.slurry          (parse only)\n\
                   \  slurry -r TEMPLATE.hs DATA.hs      (render only)\n\
                   \  slurry -b TEMPLATE.slurry DATA.hs  (parse & render)"
