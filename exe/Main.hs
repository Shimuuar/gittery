{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Main where

import Control.Monad
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Set            qualified as Set

import System.Directory
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

parser :: Parser (Dict -> IO ())
parser = subparser $ mconcat
  [ command "check" $ wrap "Check all repositories" $ do
      verbose <- switch ( short 'v'
                       <> long  "verbose"
                       <> help  "Use verbose output"
                        )
      flt <- parserFilterGrp
      pure $ traverseGroupSet (report verbose <=< checkRepositories) . flt

  , command "fetch" $ wrap "Fetch for all repositories" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet fetchRepo . flt
  , command "ff" $ wrap "Try to use ff merge from remote" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet mergeFF . flt
  , command "push" $ wrap "Try to push all changes to repository" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet pushRepo . flt
  , command "init" $ wrap "Create all missing repositories" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet cloneRepo . flt
  , command "set-remote" $ wrap "Set all remotes to values from config" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet setRemotes . flt
  , command "ls" $ wrap "List all repository groups" $ do
      flt <- parserFilterGrp
      pure $ traverseGroupSet lsRepo . flt
  ]
  where
    wrap hlp p   = (helper <*> p) `info` progDesc hlp



type Dict = Map String (RepositoryGroup FilePath)

parserFilterGrp :: Parser (Dict -> Dict)
parserFilterGrp = do
  keys <- many $ strArgument ( help "group name"
                            <> metavar "GRP")
  pure $ case keys of
    [] -> id
    _  -> \m -> Map.restrictKeys m (Set.fromList keys)

----------------------------------------------------------------
-- Command implementation
----------------------------------------------------------------

traverseGroupSet
  :: (RepositoryGroup FilePath -> IO ())
  -> (Dict                     -> IO ())
traverseGroupSet fun grps =
  forM_ (Map.toList grps) $ \(nm,grp) -> do
    reportHeader (nm ++ " [" ++ grp.host ++ "]")
    fun grp
