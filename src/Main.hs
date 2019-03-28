-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import PureScript
import Text.Parsec
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
-- import System.Console.CmdTheLine
import Control.Applicative
import System.Exit (exitSuccess, exitFailure)
-- import qualified System.IO.UTF8 as U

-- compile :: [FilePath] -> Maybe FilePath -> IO ()
-- compile inputFiles outputFile = do
--   putStrLn "compile"
  -- input <- fmap concat $ mapM U.readFile inputFiles
  -- let ast = parse parseDeclarations "" input
  -- case ast of
  --   Left err -> do
  --     U.print err
  --     exitFailure
  --   Right decls -> do
  --     case check (typeCheckAll decls) of
  --       Left typeError -> do
  --         U.putStrLn typeError
  --         exitFailure
  --       Right _ -> do
  --         let js = intercalate "\n" $ mapMaybe declToJs decls
  --         case outputFile of
  --           Just path -> U.writeFile path js
  --           Nothing -> U.putStrLn js
  --         exitSuccess



compile :: String -> IO ()
compile input  = do
  putStrLn "compile"
  let ast = parse parseDeclarations "" input
  case ast of
    Left err -> do
      putStrLn $show err
      exitFailure
    Right decls -> do
      putStrLn $ "before check: " ++ (show decls)
      case check (typeCheckAll decls) of
        Left typeError -> do
          putStrLn typeError
          exitFailure
        Right _ -> do
          let js = intercalate "\n" $ mapMaybe declToJs decls
          putStrLn js

fac  0 = 1
fac n = n * (fac $ n - 1)


insertAt :: Int-> Int-> [Int]-> [Int]
insertAt z y xs
  | z==1 = y:xs

main :: IO ()
main = compile "a = \"hahaha\" + 1;"

{-- 
-- main = compile "data A = String {a :: String } "
-- inputFiles :: Term [FilePath]
-- inputFiles = nonEmpty $ posAny [] $ posInfo
--      { posDoc = "The input .ps files" }

-- outputFile :: Term (Maybe FilePath)
-- outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
--      { optDoc = "The output .js file" }

-- term :: Term (IO ())
-- term = compile <$> inputFiles <*> outputFile

-- termInfo :: TermInfo
-- termInfo = defTI
--   { termName = "psc"
--   , version  = "1.0"
--   , termDoc  = "Compiles PureScript to Javascript"
--   }
-- main = putStrLn "hahaha"

--}