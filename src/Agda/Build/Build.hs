module Agda.Build.Build where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.Typeable
import System.Directory
import System.FilePath

import qualified Agda.Compiler.MAlonzo.Compiler as MAZ
import Agda.Interaction.FindFile
import qualified Agda.Interaction.Highlighting.HTML as HTML
import qualified Agda.Interaction.Highlighting.LaTeX as LaTeX
import Agda.Interaction.Imports hiding (typeCheck)
import Agda.Interaction.Library.Base
import Agda.Interaction.Library.Parse
import Agda.Interaction.Options
import Agda.Main
import Agda.Syntax.Abstract (toTopLevelModuleName) 
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad.Options
import Agda.TypeChecking.Monad.Signature
import Agda.TypeChecking.Monad.State
import Agda.Utils.Either
import Agda.Utils.Except
import Agda.Utils.FileName
import Paths_Agda as Paths_Agda

import Agda.Build.Files
import Agda.Build.Options


data BuildError
  = AgdaError TCErr String
  | InvalidAgdaOptions String
  deriving (Typeable)

instance Exception BuildError where

instance Show BuildError where
  show (AgdaError _ s) = s

buildTargets :: Options -> IO ()
buildTargets opts = do

  Right lib <- parseLibFile (oLibraryFile opts)

  agdaFiles <- forM (libIncludes lib) $ \dir -> do
    -- TODO: why does the `include` field include the library directory?
    let srcDir = takeDirectory baseDir </> dir
    fmap (srcDir </>) <$> findAgdaFiles srcDir

  print agdaFiles

  agdaOpts <- agdaOptions opts lib
  liftIO $ runTCMPrettyErrors' $ do
    setCommandLineOptions agdaOpts

    ifaces <- mapM typeCheck (concat agdaFiles)

    when (DocHTML `Set.member` oTargets opts) $ do
      let htmlDir = buildDir </> "doc" </> "html"
      liftIO $ createDirectoryIfMissing  True htmlDir
      -- copy css file
      cssFile <- liftIO $ Paths_Agda.getDataFileName "Agda.css"
      liftIO $ copyFile cssFile (htmlDir </> "Agda.css")

      mapM_ (generateHTMLDocs htmlDir) ifaces

    return ()

  where
    baseDir = baseDirectory opts
    buildDir = baseDir </> "_build"


typeCheck :: FilePath -> TCM Interface
typeCheck relPath = do
  m <- moduleName =<< liftIO (absolute relPath)
  (i, mw) <- getInterface' m NotMainInterface
  case mw of
    SomeWarnings ws -> tcWarningsToError ws
    NoWarnings -> return i


agdaOptions :: MonadThrow m => Options -> AgdaLibFile -> m CommandLineOptions
agdaOptions opts lib = do
  caseEitherM
{-    (liftIO $ runOptM $ parseStandardOptions
      ((bAgdaOptions $ cBuildInfo comp) ++ extraOpts))-}
    (return $ Right defaultOptions)
    (throwM . InvalidAgdaOptions)
    (\agdaOpts -> return $ agdaOpts
    -- Note: we use the original Agda files for compilation, we only copy the sources into the
    -- package for documentation purposes. Maybe we should use the copied Agda files
    -- to make sure a package is consistent? However, it would be nice if we still get
    -- the original file paths in error messages.
                        { optIncludePaths   = map (takeDirectory (baseDirectory opts) </>) (libIncludes lib)
                        --[pkgDir </> "src"]
--                        , optInterfaceDir   = Just $ pkgDir </> "ifaces"
--                        , optCompileDir     = Just $ cabalPkgDir </> "src"
                        , optDefaultLibs    = False
--                        , optLaTeXDir       = pkgDir </> "doc" </> "LaTeX"
                        }
    )

baseDirectory :: Options -> FilePath
baseDirectory = takeDirectory . oLibraryFile

generateHTMLDocs :: FilePath -> Interface -> TCM ()
generateHTMLDocs outDir i = do
  HTML.generatePage renderer outDir mod
  where
    mod = toTopLevelModuleName $ iModuleName i
    renderer css _ contents = HTML.page css mod $ HTML.code $ HTML.tokenStream contents (iHighlighting i)

-- | Run a TCM action in IO; catch, pretty print and rethrow errors.
runTCMPrettyErrors' :: (MonadIO m, MonadThrow m) => TCM a -> m a
runTCMPrettyErrors' tcm = do
  r <- liftIO $ runTCMTop' $ (Right <$> tcm) `catchError` \err -> do
    s <- prettyError err
    return $ Left $ AgdaError err s

  either throwM return r
