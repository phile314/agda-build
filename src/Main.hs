module Main where

import Control.Applicative
import Data.Monoid
import Options.Applicative

import Agda.Build.Build
import Agda.Build.Options

main :: IO ()
main = do
  opts <- execParser
    (info (helper <*> options)
      (fullDesc <> progDesc "Agda library build tool." <> header "Agda library build tool"))

  print opts
  
  buildTargets opts
  return ()
