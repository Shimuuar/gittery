{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Gittery.Commands (
  checkRepositories
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import           Data.Foldable
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import System.Directory
import System.FilePath
import System.Process

import Gittery.Types


----------------------------------------------------------------
data Result
  = Error   Text
  | RepoError Text Text
  | RepoWarn  Text Text
  deriving Show

display :: Result -> IO ()
display = print
----------------------------------------------------------------

checkRepositories :: Text -> [RepositoryTree] -> IO ()
checkRepositories hostname
  = mapM_ (mapM_ display <=< execWriterT . checkRepository hostname)

checkRepository :: Text -> RepositoryTree -> WriterT [Result] IO ()
checkRepository hostname RepositoryTree{..} =
  case hostname `HM.lookup` treeLocation of
    Nothing   -> tell [Error ("No location for host: " <> hostname)]
    Just path -> do
      -- Check for unknown nodes
      do files <- liftIO $ map T.pack <$> listDirectory path
         tell [ RepoError nm "Unknown file/directory"
              | nm <- toList $ HS.fromList files `HS.difference` HS.fromList (HM.keys treeRepos)
              ]
      -- Check repository individually
      forM_ (HM.toList treeRepos) $ \(name,Repository{..}) ->
        tell <=< liftIO $ descend path name $ do
          case repoType of
            HG  -> do out <- readProcess "hg"  ["status", "-q"]         ""
                      return [ RepoWarn name (T.pack l) | l <- lines out]
            GIT -> do out <- readProcess "git" ["status", "-s", "-uno"] ""
                      return [ RepoWarn name (T.pack l) | l <- lines out]

descend :: FilePath -> Text -> IO [Result] -> IO [Result]
descend loc repo action
  = catch (do setCurrentDirectory $ loc </> T.unpack repo
              action)
  $ \(e::IOException) -> return [RepoError repo (T.pack (show e))]
