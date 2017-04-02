module Agda.Build.Options where

import Options.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath

data Target = CompileGHC | CompileJS | CompileLaTeX | DocHTML
  deriving (Eq, Ord, Show)

data Options = Options
  { oTargets :: Set Target
  , oLibraryFile :: FilePath
  }
  deriving (Show)

options :: Parser Options
options = Options
  <$> targets
  <*> argument str (metavar "FILE")

targets :: Parser (Set Target)
targets = Set.unions <$> sequenceA
  [ tgFlag True "doc-html" DocHTML
  , tgFlag False "compile-latex" CompileLaTeX
  , tgFlag False "compile-ghc" CompileGHC
  , tgFlag False "compile-js" CompileJS
  ]
  where
    tgFlag def t g = (\x -> if x then Set.singleton g else Set.empty)
                <$> booleanFlag def (long (t)) (long ("no-" ++ t))

booleanFlag :: Bool -> Mod FlagFields Bool -> Mod FlagFields Bool -> Parser Bool
booleanFlag def yes no = (\x y -> if x || y then x && not y else def)
  <$> flag False True yes
  <*> flag False True no

