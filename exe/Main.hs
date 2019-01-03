{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Main where

import Control.Monad
import           Data.Text   (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as HM
import Network.HostName

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
  ]
  
