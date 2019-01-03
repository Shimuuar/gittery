{-# LANGUAGE LambdaCase #-}
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

data RepoErr
  = IOErr   !IOException        -- ^ IO exception during processing repository
  | RepoErr !Text               -- ^ Some error
  deriving (Show)

-- display :: Result -> IO ()
-- display = print

----------------------------------------------------------------

checkRepositories :: [(FilePath, RepositoryTree FilePath)] -> IO ()
checkRepositories = mapM_ $ \(treeName, RepositoryTree{..}) -> do
  putStrLn ("==== " ++ treeName)
  --
  realRepos <- HS.fromList . map T.pack <$> listDirectory treeLocation
  let expectedRepos = HS.fromList $ HM.keys treeRepos
  -- Missing & unexpected
  forM_ (expectedRepos `HS.difference` realRepos) $ \nm -> do
    putStrLn $ "Missing repo: " ++ T.unpack nm
  forM_ (realRepos `HS.difference` expectedRepos) $ \nm -> do
    putStrLn $ "Unexpected repo: " ++ T.unpack nm
  -- Run checks
  forM_ (HM.toList treeRepos) $ \(nm, repo) ->
    when (HS.member nm realRepos) $
      checkUncommited treeLocation nm repo >>= \case
        []   -> return ()
        errs -> do putStrLn ("* " ++ T.unpack nm)
                   mapM_ print errs


checkUncommited :: FilePath -> Text -> Repository -> IO [RepoErr]
checkUncommited tree repo Repository{..} = descend tree repo $ do
  let command = case repoType of
        HG  -> readProcess "hg"  ["status", "-q"]
        GIT -> readProcess "git" ["status", "-s", "-uno"]
  r <- command ""
  return [RepoErr $ T.pack ("Uncommited: " ++ l) | l <- lines r]
  
descend :: FilePath -> Text -> IO [RepoErr] -> IO [RepoErr]
descend loc repo action
  = catch (do setCurrentDirectory $ loc </> T.unpack repo
              action)
  $ \e -> return [IOErr e]
