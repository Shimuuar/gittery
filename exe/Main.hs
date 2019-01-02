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
import Network.HostName

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
  hostname     <- T.pack <$> getHostName
  repositories <- do
    dir  <- getXdgDirectory XdgConfig "gittery"
    cfgs <- listDirectory dir
    res  <- forM cfgs $ \c -> case takeExtension c of
      ".yaml" -> pure <$> Yaml.decodeFileEither (dir </> c)
      ".yml"  -> pure <$> Yaml.decodeFileEither (dir </> c)
      _       -> return []
    case sequence $ concat res of
      Left  e  -> error (show e)
      Right xs -> return xs
  cmd hostname repositories

parser :: Parser (Text -> [RepositoryTree] -> IO ())
parser = subparser $ mconcat
  [ command "check" $
    info (pure checkRepositories) (progDesc "Check all repositories")
  ]
  
