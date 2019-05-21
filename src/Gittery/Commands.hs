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
  , pushRepo
  , cloneRepo
  , setRemotes
  , lsRepo
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import           Data.Char (isSpace)
import           Data.List
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
  | MissingRemote !Text
  | BadRemote     !Text !Text !Text
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
    when (HS.member nm realRepos) $ do
      checkUncommited treeLocation nm repo >>= \case
        []   -> return ()
        errs -> do putStrLn ("* " ++ T.unpack nm)
                   mapM_ print errs
      checkRemotes treeLocation nm repo >>= \case
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

checkRemotes :: FilePath -> Text -> Repository -> IO [RepoErr]
checkRemotes tree repo repository@Repository{..} = descend tree repo $ do
  -- Read remotes
  remotes <- case repoType of
    HG  -> do
      names   <- readProcess "crudini" ["--get", ".hg/hgrc", "paths"] ""
      fmap HM.fromList $ forM (lines names) $ \nm -> do
        path <- readProcess "crudini" ["--get", ".hg/hgrc", "paths", nm] ""
        return (T.pack nm, T.strip $ T.pack path)
    GIT -> do
      output <- readProcess "git" ["remote", "-v"] ""
      return $ HM.fromList
        [ (T.strip nm, T.strip path)
        | line       <- T.strip . T.pack <$> lines output
        , Just clear <- [T.stripSuffix "(fetch)" line]
        , let (nm,path) = T.span (not . isSpace) clear
        ]
  --
  let expected = remoteMap repository
      missing  = HM.difference expected remotes
      wrong    = [ BadRemote nm e r
                 | (nm, (e,r)) <- HM.toList
                               $ HM.intersectionWith (,) expected remotes
                 , e /= r
                 ]
  return $ (MissingRemote <$> HM.keys missing)
        ++ wrong

descend :: FilePath -> Text -> IO [RepoErr] -> IO [RepoErr]
descend loc repo action
  = catch (do setCurrentDirectory $ loc </> T.unpack repo
              action)
  $ \e -> return [IOErr e]


----------------------------------------------------------------
-- Commands
----------------------------------------------------------------

fetchRepo :: [(FilePath, RepositoryTree FilePath)] -> ReaderT Ctx IO ()
fetchRepo = foreachRemote $ \ty (r,_) -> case ty of
  HG  -> runStdout "hg"  ["pull", T.unpack r] $ \s ->
    unless ("no changes found" `isInfixOf` s) (liftIO $ putStr s)
  GIT -> run "git" ["fetch", T.unpack r]

pushRepo :: [(FilePath, RepositoryTree FilePath)] -> ReaderT Ctx IO ()
pushRepo = foreachRemote $ \ty (r,_) -> case ty of
  -- FIXME: special case hg push (we need to treat exit code specially)
  HG  -> do
    let exe  = "hg"
        args = ["push", T.unpack r]
    asks ctxDryRun >>= \case
      True  -> liftIO $ putStrLn $ "    " ++ unwords (exe : args)
      False -> liftIO (rawSystem exe args) >>= \case
        ExitSuccess   -> return ()
        ExitFailure 1 -> return ()
        ExitFailure i -> error ("ExitFailure: " ++ show i)
  GIT -> run "git" ["push", T.unpack r, "master"]

cloneRepo :: [(FilePath, RepositoryTree FilePath)] -> IO ()
cloneRepo = mapM_ $ \(treeName, RepositoryTree{..}) -> do
  putStrLn ("==== " ++ treeName)
  --
  forM_ (HM.toList treeRepos) $ \(nm, repo@Repository{..}) -> do
    liftIO $ putStrLn ("* " ++ T.unpack nm)
    let dir = treeLocation </> T.unpack nm
    doesDirectoryExist dir >>= \case
      True  -> return ()
      False -> do
        createDirectoryIfMissing True dir
        setCurrentDirectory dir
        case repoType of
          HG  -> do run' "hg" ["init"]
                    forM_ (remoteList repo) $ \(r,url) ->
                      run' "crudini" ["--set", ".hg/hgrc", "paths", T.unpack r, T.unpack url]
          GIT -> do run' "git" ["init"]
                    forM_ (remoteList repo) $ \(r,url) ->
                      run' "git" ["remote", "add", T.unpack r, T.unpack url]

setRemotes :: [(FilePath, RepositoryTree FilePath)] -> IO ()
setRemotes = mapM_ $ \(treeName, RepositoryTree{..}) -> do
  putStrLn ("==== " ++ treeName)
  --
  forM_ (HM.toList treeRepos) $ \(nm, repo@Repository{..}) -> do
    liftIO $ putStrLn ("* " ++ T.unpack nm)
    let dir = treeLocation </> T.unpack nm
    doesDirectoryExist dir >>= \case
      False -> putStrLn "Missing repo"
      True  -> do
        setCurrentDirectory dir
        case repoType of
          HG  -> do forM_ (remoteList repo) $ \(r,url) ->
                      run' "crudini" ["--set", ".hg/hgrc", "paths", T.unpack r, T.unpack url]
          GIT -> do remotes <- map T.pack . lines <$> readProcess "git" ["remote"] ""
                    forM_ (remoteList repo) $ \(r,url) -> do
                      let cmd = if r `elem` remotes then "set-url" else "add"
                      run' "git" ["remote", cmd, T.unpack r, T.unpack url]

lsRepo :: [(FilePath, RepositoryTree FilePath)] -> IO ()
lsRepo = mapM_ $ \(treeName, _) -> putStrLn treeName



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

run' :: String -> [String] -> IO ()
run' exe args =
  rawSystem exe args >>= \case
    ExitSuccess   -> return ()
    ExitFailure i -> error ("ExitFailure: " ++ show i)

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
