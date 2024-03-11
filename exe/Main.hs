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
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Set            qualified as Set
import Data.Text           qualified as T
import Data.Yaml           qualified as Yaml
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
  -- Parse command line parameters
  cmd <- customExecParser (prefs showHelpOnError)
       $ info (helper <*> parser)
              (  fullDesc
              <> header   "Gittery"
              <> progDesc "tool for managing repositories"
              )
  -- Load config and execute
  cfg <- toLocalConfig
     =<< readConfig
     =<< getXdgDirectory XdgConfig "gittery"
  cmd cfg


-- filterArguments :: [String] -> [(FilePath, a)] -> [(FilePath, a)]
-- filterArguments []   xs = xs
-- filterArguments keys xs
--   | not (Set.null unknownKeys) = error $ unwords $ "Unknown repo trees:" : Set.toList unknownKeys
--   | otherwise                  = filter (\(k,_) -> k `Set.member` keySet) xs
--   where
--     keySet       = Set.fromList keys
--     existingKeys = Set.fromList $ map fst xs
--     unknownKeys  = keySet `Set.difference` existingKeys

parser :: Parser (Map String (RepositoryGroup FilePath) -> IO ())
parser = subparser $ mconcat
  [ command "check" $ wrap "Check all repositories"     $ do
      -- keys <- keyParser
      -- pure (checkRepositories . filterArguments keys)
      pure $ \grps -> forM_ (Map.toList grps) $ \(nm,grp) -> do
        reportHeader nm
        report =<< checkRepositories grp
  , command "fetch" $ wrap "Fetch for all repositories" $ do
      pure $ \grps -> forM_ (Map.toList grps) $ \(nm,grp) -> do
        reportHeader nm
        fetchRepo grp
--       ctxRepoParams <- ignoreParser
--       ctxDryRun     <- dryRunParser
--       keys          <- keyParser
--       pure $ flip runReaderT Ctx{..} . fetchRepo . filterArguments keys
--   , command "push" $ wrap "Try to push all changes to repository" $ do
--       ctxRepoParams <- ignoreParser
--       ctxDryRun     <- dryRunParser
--       keys          <- keyParser
--       pure $ flip runReaderT Ctx{..} . pushRepo . filterArguments keys
  , command "init" $ wrap "Create all missing repositories" $ do
      pure $ \grps -> forM_ (Map.toList grps) $ \(nm,grp) -> do
        reportHeader nm
        cloneRepo grp
  , command "set-remote" $ wrap "Set all remotes to values from config" $ do
      pure $ \grps -> forM_ (Map.toList grps) $ \(nm,grp) -> do
        reportHeader nm
        setRemotes grp
  , command "ls"   $ wrap "List all repository groups"      $ pure lsRepo
  ]
  where
    wrap hlp p   = (helper <*> p) `info` progDesc hlp
--     --
--     ignoreParser =  many $ asum
--       [ IgnoreRemoteName   <$> strOption ( long "ignore-remote-name"
--                                         <> help "Ignore remote by its name"
--                                          )
--       , IgnoreRemotePrefix <$> strOption ( long "ignore-remote-prefix"
--                                         <> help "Ignore remote by its name"
--                                          )
--       , IgnoreRemoteInfix  <$> strOption ( long "ignore-remote-infix"
--                                         <> help "Ignore remote by its name"
--                                         )
--       ]
--     dryRunParser = switch ( long "dry-run"
--                          <> help "Do nothing"
--                           )
--     keyParser =  many $ strArgument ( help    "Repo trees to check"
--                                    <> metavar "TREE"
                                    -- )
