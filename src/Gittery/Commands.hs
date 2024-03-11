{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
module Gittery.Commands
  ( -- * Reports
    Report(..)
  , reportHeader
  , report
    -- * Actions
  , checkRepositories
  , lsRepo
  , setRemotes
  , fetchRepo
  -- , pushRepo
  , cloneRepo
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Char (isSpace)
-- import Data.List
import Data.Text qualified as T
import Data.Text (Text)
import Data.HashMap.Strict qualified as HM
import Data.HashSet        qualified as HS
import Data.Map.Strict     qualified as Map
import Data.Map.Strict     (Map)
import Data.Foldable
import Data.Traversable
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.Console.ANSI qualified as Term

import Gittery.Types


----------------------------------------------------------------

-- -- | Flags for ignoring repositories and such
-- data RepoParams
--   = IgnoreRemoteName   !Text
--   | IgnoreRemotePrefix !Text
--   | IgnoreRemoteInfix  !Text
--   deriving (Show)

-- data Ctx = Ctx
--   { ctxRepoParams :: [RepoParams]
--   , ctxDryRun     :: Bool
--   }
--   deriving (Show)

-- ----------------------------------------------------------------

-- | Error that occured 
data GitErr
  = IOErr   !IOException
    -- ^ IO exception during processing repository
  | MissingRepository
    -- ^ Repository is missing
  | MissingRemote !Text !Text
    -- ^ Remote is missing from repository
  | BadRemote     !Text !Text !Text
    -- ^ Remote has incorrect URL
  | Uncommited FilePath
    -- ^ Uncommited file
  deriving (Show)

data Report a = Report
  { warn :: [a]
  , errs :: [a]
  }
  deriving stock (Show)

pattern OK = Report [] []

instance Semigroup (Report a) where
  a <> b = Report { warn = a.warn <> b.warn
                  , errs = a.errs <> b.errs
                  }
instance Monoid (Report a) where
  mempty = Report [] []

reportHeader :: String -> IO ()
reportHeader s = do
  putStr "====  "
  putStr s
  putStrLn "  ===="

report :: Map String (Report GitErr) -> IO ()
report (Map.toList -> reps) =
  forM_ reps $ \(nm,r) -> do
    putStr nm >> putStr (replicate (n + 4 - length nm) ' ')
    case r of
      Report [] [] -> do
        Term.setSGR [Term.SetColor Term.Foreground Term.Vivid Term.Green]
        putStrLn "OK"
        Term.setSGR [Term.Reset]
      Report warn []   -> do
        withColor Term.Yellow $ putStrLn "WARN"
        reportErr Term.Yellow warn
      Report _    errs -> do
        withColor Term.Red $ putStrLn "ERROR"
        reportErr Term.Red errs
  where
    n = maximum $ map (length . fst) reps
    reportErr col errs = do
      forM_ errs $ \err -> case pprGitErr err of
        []     -> pure ()
        (s:ss) -> do
          putStr "  * "
          withColor col $ do
            putStrLn s
            mapM_ (putStrLn . ("    " ++)) ss
    --
    withColor col action = do
      Term.setSGR [Term.SetColor Term.Foreground Term.Vivid col]
      action
      Term.setSGR [Term.Reset]

pprGitErr :: GitErr -> [String]
pprGitErr = \case
  IOErr e -> ["IO exception: " <> show e]
  MissingRepository -> ["Missing repository"]
  MissingRemote nm url -> ["Missing remote " <> T.unpack nm <> ": " <> T.unpack url]
  BadRemote nm e r ->
    [ "Bad remote " <> T.unpack nm
    , " expected: " <> T.unpack e
    , " real:     " <> T.unpack r
    ]
  Uncommited nm -> ["Uncommited: " ++ nm]

----------------------------------------------------------------
-- Checking repositories
----------------------------------------------------------------

-- | Check group of repositories
checkRepositories
  :: RepositoryGroup FilePath
  -> IO (Map String (Report GitErr))
checkRepositories grp = flip Map.traverseWithKey grp.repos $ \k r -> do
  let dir = grp.host </> k
  doesDirectoryExist dir >>= \case
    False -> pure Report{warn=[], errs=[MissingRepository]}
    True  -> checkRepository dir r
--   forM_ (expectedRepos `HS.difference` realRepos) $ \nm -> do
--     putStrLn $ "Missing repo: " ++ T.unpack nm
--   forM_ (realRepos `HS.difference` expectedRepos) $ \nm -> do
--     putStrLn $ "Unexpected repo: " ++ T.unpack nm

-- | Check single repository
checkRepository
  :: FilePath   -- ^ Directory with a repo
  -> Repository -- ^ Repository description
  -> IO (Report GitErr)
checkRepository path repo = captureIOErr $ do
  setCurrentDirectory path
  gitCheckRemotes repo <> gitCheckUncommited

-- | List know repositories
lsRepo :: Map String (RepositoryGroup FilePath) -> IO ()
lsRepo reposet = forM_ (Map.toList reposet) $ \(nm, repo) -> do
  reportHeader (nm ++ " [" ++ repo.host ++ "]")
  forM_ (Map.keys repo.repos) $ \k -> putStrLn ("  * " ++ k)

-- | Set correct remotes for all repositories
setRemotes :: RepositoryGroup FilePath
           -> IO ()
setRemotes grp = forM_ (Map.toList grp.repos) $ \(k,repo) -> do
  let dir = grp.host </> k
  doesDirectoryExist dir >>= \case
    False -> pure ()
    True  -> do
      setCurrentDirectory dir
      remotes <- gitRemotes
      let expected = repo.remote
          missing = HM.toList $ HM.difference expected remotes
          wrong   = [ (nm, e)
                    | (nm, (e,r)) <- HM.toList
                                   $ HM.intersectionWith (,) expected remotes
                    , e /= r
                    ]
      when (not (null missing) || not (null wrong)) $ do
        putStrLn $ "  * " ++ k
        forM_ missing $ \(nm,url) -> do
          runCommandVerbose "git" ["remote", "add", T.unpack nm, T.unpack url]
        forM_ wrong   $ \(nm,url) -> do
          runCommandVerbose "git" ["remote", "set-url", T.unpack nm, T.unpack url]

cloneRepo :: RepositoryGroup FilePath -> IO ()
cloneRepo grp = forM_ (Map.toList grp.repos) $ \(k,repo) -> do
  let dir = grp.host </> k
  doesDirectoryExist dir >>= \case
    True -> pure ()
    False -> do
      putStrLn $ "  * " ++ k
      createDirectoryIfMissing True dir
      setCurrentDirectory dir
      runCommandVerbose "git" ["init"]
      forM_ (HM.toList repo.remote) $ \(r,url) -> do
        runCommandVerbose "git" ["remote", "add", T.unpack r, T.unpack url]
        runCommandVerbose "git" ["fetch", T.unpack r]

fetchRepo :: RepositoryGroup FilePath -> IO ()
fetchRepo grp = forM_ (Map.toList grp.repos) $ \(k,repo) -> do
  let dir = grp.host </> k
  doesDirectoryExist dir >>= \case
    False -> pure ()
    True  -> do
      putStrLn $ "  * " ++ k
      setCurrentDirectory dir
      forM_ (HM.toList repo.remote) $ \(r,url) -> do
        runCommandVerbose "git" ["fetch", T.unpack r]


----------------------------------------------------------------
-- Git interactions
----------------------------------------------------------------

gitRemotes :: IO (HM.HashMap Text Text)
gitRemotes = do
  output <- readProcess "git" ["remote", "--verbose"] ""
  -- NOTE: Here we assume that (fetch/push) remotes are identical
  return $ HM.fromList
    [ (T.strip nm, T.strip path)
    | line       <- T.strip . T.pack <$> lines output
    , Just clear <- [T.stripSuffix "(fetch)" line]
    , let (nm,path) = T.span (not . isSpace) clear
    ]

-- | Check for uncommmited files in repository
gitCheckUncommited :: IO (Report GitErr)
gitCheckUncommited = do
  r <- readProcess "git" ["status", "-s", "-uno"] ""
  return Report{ warn = Uncommited <$> lines r
               , errs = []
               }

-- | Check remotes of a repository in current working dir
gitCheckRemotes :: Repository -> IO (Report GitErr)
gitCheckRemotes repo = do
  remotes <- gitRemotes
  let expected = repo.remote
      missing  = [ MissingRemote nm url
                 | (nm,url) <- HM.toList $ HM.difference expected remotes
                 ]
      wrong    = [ BadRemote nm e r
                 | (nm, (e,r)) <- HM.toList
                                $ HM.intersectionWith (,) expected remotes
                 , e /= r
                 ]
  pure Report { warn = []
              , errs = missing ++ wrong
              }

captureIOErr :: IO (Report GitErr) -> IO (Report GitErr)
captureIOErr = handle $ \e ->
  pure Report { warn = []
              , errs = [IOErr e]
              }

runCommandVerbose
  :: String -> [String] -> IO ()
runCommandVerbose cmd args = do
  putStr "$ "
  putStr cmd
  forM_ args $ \a -> putStr (' ':show a)
  putStrLn ""
  run' cmd args





-- ----------------------------------------------------------------
-- -- Utils
-- ----------------------------------------------------------------

-- foreachRemote
--   :: (RepoType -> (Text,Text) -> ReaderT Ctx IO ())
--      -- ^ Action for each remote
--   -> [(FilePath, RepositoryTree FilePath)]
--      -- ^ List of repositories
--   -> ReaderT Ctx IO ()
-- foreachRemote action = mapM_ $ \(treeName, RepositoryTree{..}) -> do
--   params <- asks ctxRepoParams
--   liftIO $ putStrLn ("==== " ++ treeName)
--   --
--   forM_ (HM.toList treeRepos) $ \(nm, repo@Repository{..}) -> do
--     liftIO $ putStrLn ("* " ++ T.unpack nm)
--     liftIO $ setCurrentDirectory $ treeLocation </> T.unpack nm
--     mapM_ (action repoType)
--       $ filter (acceptRemote params)
--       $ remoteList repo


-- acceptRemote :: [RepoParams] -> (Text,Text) -> Bool
-- acceptRemote params (remote,url)
--   = all (not . reject) params
--   where
--     reject (IgnoreRemoteName   nm)   = nm == remote
--     reject (IgnoreRemotePrefix prfx) = prfx `T.isPrefixOf` url
--     reject (IgnoreRemoteInfix  infx) = infx `T.isInfixOf`  url

-- remoteList :: Repository -> [(Text,Text)]
-- remoteList Repository{..} = case remote of
--   RemoteMany   rs -> HM.toList rs
--   RemoteSimple r  -> case repoType of
--     HG  -> [("default", r)]
--     GIT -> [("origin",  r)]

run' :: String -> [String] -> IO ()
run' exe args =
  rawSystem exe args >>= \case
    ExitSuccess   -> return ()
    ExitFailure i -> error ("ExitFailure: " ++ show i)

-- run :: String -> [String] -> ReaderT Ctx IO ()
-- run exe args =
--   asks ctxDryRun >>= \case
--     True  -> liftIO $ putStrLn $ "    " ++ unwords (exe : args)
--     False -> liftIO (rawSystem exe args) >>= \case
--       ExitSuccess   -> return ()
--       ExitFailure i -> error ("ExitFailure: " ++ show i)

-- runStdout :: String -> [String] -> (String -> ReaderT Ctx IO ()) -> ReaderT Ctx IO ()
-- runStdout exe args cont =
--   asks ctxDryRun >>= \case
--     True  -> liftIO $ putStrLn $ "    " ++ unwords (exe : args)
--     False -> cont =<< liftIO (readProcess exe args "")
