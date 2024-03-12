{-# LANGUAGE DeriveAnyClass      #-}
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
  , mergeFF
  , pushRepo
  , cloneRepo
  ) where

import Control.Exception
import Control.Monad
import Data.Char                (isSpace)
import Data.Text                qualified as T
import Data.Text.Lazy           qualified as TL
import Data.Text                (Text)
import Data.Text.Lazy.Encoding  qualified as TL
import Data.Text.Encoding.Error qualified as T
import Data.HashMap.Strict      qualified as HM
import Data.Map.Strict          qualified as Map
import Data.Map.Strict          (Map)
import System.Directory
import System.FilePath
import System.Process.Typed
import System.Console.ANSI qualified as Term

import Gittery.Types

----------------------------------------------------------------
-- Reports
----------------------------------------------------------------

-- | Error that occured
data GitErr
  = IOErr   !IOException
    -- ^ IO exception during processing repository
  | GitErr  [String]
    -- ^ Git error
  | MissingRepository
    -- ^ Repository is missing
  | UnknownRepository FilePath
    -- ^ There's exists repository not listed it config
  | MissingRemote !Text !Text
    -- ^ Remote is missing from repository
  | BadRemote     !Text !Text !Text
    -- ^ Remote has incorrect URL
  | Uncommited FilePath        -- ^ Uncommited file
  | LocalBranchAhead  String String
  | LocalBranchBehind String String
  | BranchesDiverged  String String
  deriving stock (Show)
  deriving anyclass Exception

data Report a = Report
  { warn :: [a]
  , errs :: [a]
  }
  deriving stock (Show)

pattern OK :: Report a
pattern OK = Report [] []

pattern Warn :: a -> Report a
pattern Warn a = Report [a] []

pattern Warns :: [a] -> Report a
pattern Warns a = Report a []

instance Semigroup (Report a) where
  a <> b = Report { warn = a.warn <> b.warn
                  , errs = a.errs <> b.errs
                  }
instance Monoid (Report a) where
  mempty = Report [] []

reportHeader :: String -> IO ()
reportHeader s = do
  Term.setSGR [ Term.SetColor Term.Foreground Term.Vivid Term.White
              , Term.SetConsoleIntensity Term.BoldIntensity
              ]
  putStrLn $ "====  " ++ s ++ "  ===="
  Term.setSGR [ Term.Reset ]

report :: Bool -> Map String (Report GitErr) -> IO ()
report verbose (Map.toList -> reps) = forM_ reps $ \case
  -- Empty string correponds to warnings correponding to groups.
  -- And conveniently it comes first!
  ("", OK)               -> pure ()
  ("", Report warn errs) -> do
    reportErr Term.Yellow warn
    reportErr Term.Red    errs
  -- Normal repos
  (_ , OK) | not verbose -> pure ()
  (nm,r) -> do
    putStr nm >> putStr (replicate (n + 1 - length nm) ' ')
    case r of
      OK ->
        withColor Term.Green $ putStrLn "OK"
      Report warn [] -> do
        withColor Term.Yellow $ putStrLn "WARN"
        reportErr Term.Yellow warn
      Report _    errs -> do
        withColor Term.Red $ putStrLn "ERROR"
        reportErr Term.Red errs
  where
    n = max 24 $ maximum $ map (length . fst) reps
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
      _ <- action
      Term.setSGR [Term.Reset]

pprGitErr :: GitErr -> [String]
pprGitErr = \case
  IOErr  e             -> ["IO exception: " <> show e]
  GitErr e             -> e
  MissingRepository    -> ["Missing repository"]
  UnknownRepository r  -> ["UnknownRepository " ++ show r]
  MissingRemote nm url -> ["Missing remote " <> T.unpack nm <> ": " <> T.unpack url]
  BadRemote nm e r ->
    [ "Bad remote " <> T.unpack nm
    , " expected: " <> T.unpack e
    , " real:     " <> T.unpack r
    ]
  Uncommited nm -> ["Uncommited: " ++ nm]
  LocalBranchAhead  br remote -> ["Local branch '"<>br<> "' is ahead of '"<>remote<>"'"]
  LocalBranchBehind br remote -> ["Local branch '"<>br<> "' is behind '"<>remote<>"'"]
  BranchesDiverged  br remote -> ["Branches '"<>br<> "' and '"<>remote<>"' diverged"]

----------------------------------------------------------------
-- Checking repositories
----------------------------------------------------------------

-- | Check group of repositories
checkRepositories
  :: RepositoryGroup FilePath
  -> IO (Map String (Report GitErr))
checkRepositories grp = mconcat
  [ flip Map.traverseWithKey grp.repos $ \k r -> do
      let dir = grp.host </> k
      doesDirectoryExist dir >>= \case
        False -> pure Report{warn=[], errs=[MissingRepository]}
        True  -> checkRepository dir r
    -- FIXME: We're using hack by representing "" as related to group as whole
  , if grp.no_unknown
    then do dirs <- listDirectory grp.host
            return $ Map.singleton "" Report
              { warn = [ UnknownRepository nm
                       | nm <- dirs
                       , nm `Map.notMember` grp.repos
                       ]
              , errs = []
              }
    else do pure mempty
  ]

-- | Check single repository
checkRepository
  :: FilePath   -- ^ Directory with a repo
  -> Repository -- ^ Repository description
  -> IO (Report GitErr)
checkRepository path repo = captureIOErr $ do
  setCurrentDirectory path
  -- We want to capture each error separately in order to avoid single
  -- exception clobbering all checks
  mconcat
    [ captureIOErr $ gitCheckRemotes repo
    , captureIOErr $ gitCheckUncommited
    , captureIOErr $ gitCheckBranch repo
    ]

-- | List know repositories
lsRepo :: RepositoryGroup FilePath -> IO ()
lsRepo repo = forM_ (Map.keys repo.repos) $ \k -> putStrLn ("  * " ++ k)

-- | Set correct remotes for all repositories
setRemotes :: RepositoryGroup FilePath
           -> IO ()
setRemotes = traverseExistingRepo_ $ \nm repo -> do
  remotes <- gitRemotes
  let expected = repo.remote
      missing  = HM.toList $ HM.difference expected remotes
      wrong    = [ (k, e)
                 | (k, (e,r)) <- HM.toList
                               $ HM.intersectionWith (,) expected remotes
                 , e /= r
                 ]
  when (not (null missing) || not (null wrong)) $ do
    putStrLn $ "  * " ++ nm
    forM_ missing $ \(k,url) -> do
      runCommandVerbose "git" ["remote", "add", T.unpack k, T.unpack url]
    forM_ wrong   $ \(k,url) -> do
      runCommandVerbose "git" ["remote", "set-url", T.unpack k, T.unpack url]

-- | Clone and fetch missing repository
cloneRepo :: RepositoryGroup FilePath -> IO ()
cloneRepo = traverseMissingRepo_ $ \dir nm repo -> do
  putStrLn $ "  * " ++ nm
  createDirectoryIfMissing True dir
  setCurrentDirectory dir
  runCommandVerbose "git" ["init"]
  forM_ (HM.toList repo.remote) $ \(r,url) -> do
    runCommandVerbose "git" ["remote", "add", T.unpack r, T.unpack url]
    runCommandVerbose "git" ["fetch", T.unpack r]

-- | Fetch from each remote
fetchRepo :: RepositoryGroup FilePath -> IO ()
fetchRepo = traverseExistingRepo_ $ \nm repo -> do
  putStrLn $ "  * " ++ nm
  forM_ (HM.toList repo.remote) $ \(r,_url) -> do
    runCommandVerbose "git" ["fetch", T.unpack r]

-- | Attempt to use ff merge
mergeFF :: RepositoryGroup FilePath -> IO ()
mergeFF = traverseExistingRepo_ $ \nm repo -> do
  putStrLn $ "  * " ++ nm
  gitUncommited >>= \case
    (_:_) -> pure () -- Skip if there're uncommmited changes
    []    -> do
      active_br <- gitCurrentBranch
      forM_ (T.unpack <$> repo.branches) $ \br ->
        forM_ (T.unpack <$> HM.keys repo.remote) $ \remote -> do
          let remote_br = remote<>"/"<>br
          gitIsSameRevision br remote_br >>= \case
            True  -> pure ()
            False -> br `gitIsAncestor` remote_br >>= \case
              False -> pure ()
              True
                | br == active_br -> runCommandVerbose "git" ["merge", "--ff-only", remote_br]
                | otherwise       -> runCommandVerbose "git" ["fetch", remote, br++":"++br]

pushRepo :: RepositoryGroup FilePath -> IO ()
pushRepo = traverseExistingRepo_ $ \nm repo -> case repo.can_push of
  []   -> pure ()
  push -> do
    putStrLn $ "  * " ++ nm
    forM_ (T.unpack <$> push) $ \br ->
      forM_ (T.unpack <$> HM.keys repo.remote) $ \remote -> do
        let remote_br = remote<>"/"<>br
        gitIsSameRevision br remote_br >>= \case
          True  -> pure ()
          False -> remote_br `gitIsAncestor` br >>= \case
            False -> pure ()
            True  -> runCommandVerbose "git" ["push", remote, br]

----------------------------------------------------------------
-- Git interactions
----------------------------------------------------------------

gitRemotes :: IO (HM.HashMap Text Text)
gitRemotes = do
  output <- readGitOutput ["remote", "--verbose"]
  -- NOTE: Here we assume that (fetch/push) remotes are identical
  return $ HM.fromList
    [ (T.strip nm, T.strip path)
    | line       <- T.strip . T.pack <$> lines output
    , Just clear <- [T.stripSuffix "(fetch)" line]
    , let (nm,path) = T.span (not . isSpace) clear
    ]

-- | Check for uncommmited files in repository
gitCheckUncommited :: IO (Report GitErr)
gitCheckUncommited = Warns . fmap Uncommited <$> gitUncommited

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

gitCheckBranch :: Repository -> IO (Report GitErr)
gitCheckBranch repo = mconcat
  [ gitIsSameRevision br remote_br >>= \case
      True  -> mempty
      False -> br `gitIsAncestor` remote_br >>= \case
        True  -> pure $ Warn $ LocalBranchBehind br remote_br
        False -> remote_br `gitIsAncestor` br >>= \case
          True  -> pure $ Warn $ LocalBranchAhead br remote_br
          False -> pure $ Warn $ BranchesDiverged br remote_br
  | br     <- T.unpack <$> repo.branches
  , remote <- T.unpack <$> HM.keys repo.remote
  , let remote_br = remote<>"/"<>br
  ]

captureIOErr :: IO (Report GitErr) -> IO (Report GitErr)
captureIOErr = flip catches
  [ Handler $ \e             -> pure $ Report [] [IOErr e]
  , Handler $ \(e :: GitErr) -> pure $ Report [] [e]
  ]

-- | Run command and write its output to terminal
runCommandVerbose :: String -> [String] -> IO ()
runCommandVerbose cmd args = do
  putStrLn $ "$ " <> cmd <> concatMap (\a -> ' ':escape a) args
  runProcess (proc cmd args) >>= \case
    ExitSuccess   -> return ()
    ExitFailure i -> error ("ExitFailure: " ++ show i)
  where
    -- Naive string excaping
    escape s | any special s = show s
             | otherwise     = s
    special c = isSpace c || c == '!' || c == '$' || c == '#'

-- | Read git output
readGitOutput :: [String] -> IO String
readGitOutput args = do
  (code,out,err) <- readProcess $ proc "git" args
  case code of
    ExitSuccess   -> pure $ decoder out
    ExitFailure n -> throwIO $ GitErr
                   $ ("Git exited with error code " ++ show n)
                   : lines (decoder err)
  where
    decoder = TL.unpack . TL.decodeUtf8With T.strictDecode

-- | Check if two branches reference sme revision
gitIsSameRevision :: String -> String -> IO Bool
gitIsSameRevision ref1 ref2 = do
  hash1 <- readGitOutput ["rev-parse", ref1, "--"]
  hash2 <- readGitOutput ["rev-parse", ref2, "--"]
  return $! hash1 == hash2

-- | Check if ref1 is ancestor of ref2
gitIsAncestor :: String -> String -> IO Bool
gitIsAncestor ref1 ref2 =
  runProcess (proc "git" ["merge-base", "--is-ancestor", ref1, ref2]) >>= \case
    ExitSuccess   -> pure True
    ExitFailure 1 -> pure False
    ExitFailure i -> error ("ExitFailure: " ++ show i)

-- | List uncommited changes ignoring untracked files
gitUncommited :: IO [String]
gitUncommited = lines <$> readGitOutput ["status", "-s", "-uno"]

gitCurrentBranch :: IO String
gitCurrentBranch
  =  reverse . dropWhile (\c -> c=='\n' || c=='\r') . reverse
 <$> readGitOutput ["rev-parse", "--abbrev-ref", "HEAD"]

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


----------------------------------------------------------------
-- Traversals
----------------------------------------------------------------

-- | Visit eah repository which already exists
traverseExistingRepo_
  :: (String -> Repository -> IO a)
  -> RepositoryGroup FilePath
  -> IO ()
traverseExistingRepo_ go grp =
  forM_ (Map.toList grp.repos) $ \(nm,repo) -> do
    let dir = grp.host </> nm
    doesDirectoryExist dir >>= \case
      False -> pure ()
      True  -> do setCurrentDirectory dir
                  void $ go nm repo

-- | Visit eah repository which already exists
traverseMissingRepo_
  :: (FilePath -> String -> Repository -> IO a)
  -> RepositoryGroup FilePath
  -> IO ()
traverseMissingRepo_ go grp =
  forM_ (Map.toList grp.repos) $ \(nm,repo) -> do
    let dir = grp.host </> nm
    doesDirectoryExist dir >>= \case
      False -> void $ go dir nm repo
      True  -> pure ()
