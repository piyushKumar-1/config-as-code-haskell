{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib as L
import qualified Data.Aeson as A
import System.Environment


main :: IO ()
main = do 
  args <- getArgs
  let (filepath, distance) = 
        case args of 
          [a, b] -> (a, read b)
          _ -> error "wrong input arguments"
  file :: Either String L.FareConfig <- A.eitherDecodeFileStrict filepath
  case file of 
    Left err -> print err 
    Right fareConfig -> do 
      res <- L.calculateFare fareConfig distance (finalFare fareConfig) 
      print res
















































