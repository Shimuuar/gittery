{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Main where

import Control.Monad
import Data.Foldable
import Control.Monad.Trans.Reader
import           Data.Text   (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as HM
import Network.HostName

import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import Options.Applicative

import Gittery.Types
import Gittery.Commands

----------------------------------------------------------------
--
----------------------------------------------------------------

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnError)
       $ info (helper <*> parser)
              (  fullDesc
              <> header   "Gittery"
              <> progDesc "tool for managing repositories"
              )
  --
  unsetEnv "LANG"
  hostname     <- getHostName
  repositories <- do
    dir  <- getXdgDirectory XdgConfig "gittery"
    cfgs <- listDirectory dir
    fmap concat $ forM cfgs $ \c -> do
      let ext    = takeExtension c
          isYaml = ext == ".yml" || ext == ".yaml"
      case isYaml of
        False -> return []
        True  -> Yaml.decodeFileEither (dir </> c) >>= \case
          Left  e -> do putStrLn ("Cannot decode configuration file " ++ c)
                        print e
                        exitFailure
          Right x -> case traverse (HM.lookup (T.pack hostname)) x of
            Nothing -> do putStrLn ("No entry for hostname `"++hostname++"' in "++c)
                          exitFailure
            Just y  -> return [(c, y)]
  --
  cmd repositories



parser :: Parser ([(FilePath, RepositoryTree FilePath)] -> IO ())
parser = subparser $ mconcat
  [ command "check" $
    info (pure checkRepositories) (progDesc "Check all repositories")
  , command "fetch" $
    flip info (progDesc "Fetch for all repositories") $ helper <*> do
      ctxRepoParams <- many $ asum
        [ IgnoreRemoteName   <$> strOption ( long "ignore-remote-name"
                                          <> help "Ignore remote by its name"
                                           )
        , IgnoreRemotePrefix <$> strOption (long "ignore-remote-prefix"
                                         <> help "Ignore remote by its name"
                                          )
        , IgnoreRemoteInfix  <$> strOption (long "ignore-remote-infix"
                                         <> help "Ignore remote by its name"
                                         )
        ]
      ctxDryRun <- switch (  long "dry-run"
                          <> help "Do nothing")
      pure $ flip runReaderT Ctx{..} . fetchRepo
  ]
  
