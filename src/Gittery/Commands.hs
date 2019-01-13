{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Gittery.Commands (
    RepoParams(..)
  , Ctx(..)
  , checkRepositories
  , fetchRepo
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import           Data.List
import           Data.Foldable
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import System.Directory
import System.FilePath
import System.Process
import System.Exit

import Gittery.Types


----------------------------------------------------------------

-- | Flags for ignoring repositories and such
data RepoParams
  = IgnoreRemoteName   !Text
  | IgnoreRemotePrefix !Text
  | IgnoreRemoteInfix  !Text
  deriving (Show)

data Ctx = Ctx
  { ctxRepoParams :: [RepoParams]
  , ctxDryRun     :: Bool
  }
  deriving (Show)

----------------------------------------------------------------

data RepoErr
  = IOErr   !IOException        -- ^ IO exception during processing repository
  | RepoErr !Text               -- ^ Some error
  deriving (Show)



----------------------------------------------------------------
-- Checking repositories
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


----------------------------------------------------------------
-- Fetching
----------------------------------------------------------------

fetchRepo :: [(FilePath, RepositoryTree FilePath)] -> ReaderT Ctx IO ()
fetchRepo = foreachRemote $ \ty (r,_) -> case ty of
  HG  -> runStdout "hg"  ["pull", T.unpack r] $ \s ->
    unless ("no changes found" `isInfixOf` s) (liftIO $ putStr s)
  GIT -> run "git" ["fetch", T.unpack r]



----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

foreachRemote
  :: (RepoType -> (Text,Text) -> ReaderT Ctx IO ())
     -- ^ Action for each remote
  -> [(FilePath, RepositoryTree FilePath)]
     -- ^ List of repositories
  -> ReaderT Ctx IO ()
foreachRemote action = mapM_ $ \(treeName, RepositoryTree{..}) -> do
  params <- asks ctxRepoParams
  liftIO $ putStrLn ("==== " ++ treeName)
  --
  forM_ (HM.toList treeRepos) $ \(nm, repo@Repository{..}) -> do
    liftIO $ putStrLn ("* " ++ T.unpack nm)
    liftIO $ setCurrentDirectory $ treeLocation </> T.unpack nm
    mapM_ (action repoType)
      $ filter (acceptRemote params)
      $ remoteList repo


acceptRemote :: [RepoParams] -> (Text,Text) -> Bool
acceptRemote params (remote,url)
  = all (not . reject) params
  where
    reject (IgnoreRemoteName   nm)   = nm == remote
    reject (IgnoreRemotePrefix prfx) = prfx `T.isPrefixOf` url
    reject (IgnoreRemoteInfix  infx) = infx `T.isInfixOf`  url

remoteList :: Repository -> [(Text,Text)]
remoteList Repository{..} = case remote of
  RemoteMany   rs -> HM.toList rs
  RemoteSimple r  -> case repoType of
    HG  -> [("default", r)]
    GIT -> [("origin",  r)]

run :: String -> [String] -> ReaderT Ctx IO ()
run exe args =
  asks ctxDryRun >>= \case
    True  -> liftIO $ putStrLn $ "    " ++ unwords (exe : args)
    False -> liftIO (rawSystem exe args) >>= \case
      ExitSuccess   -> return ()
      ExitFailure i -> error ("ExitFailure: " ++ show i)

runStdout :: String -> [String] -> (String -> ReaderT Ctx IO ()) -> ReaderT Ctx IO ()
runStdout exe args cont =
  asks ctxDryRun >>= \case
    True  -> liftIO $ putStrLn $ "    " ++ unwords (exe : args)
    False -> cont =<< liftIO (readProcess exe args "")
