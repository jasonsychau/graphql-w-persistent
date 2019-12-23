#!/usr/bin/env stack
module Main where

import Lib (runQuery)
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "Query and variables arguments are missing."
      (qry:[]) -> do 
                    rlt <- runQuery qry ""
                    print rlt
      (qry:vars:[]) -> do
                        rlt <- runQuery qry vars
                        print rlt
      _ -> error "too many args"
