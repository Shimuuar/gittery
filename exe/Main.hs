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
import qualified Data.Text as T
import qualified Data.Set  as Set
import qualified Data.Yaml as Yaml
import qualified Data.HashMap.Strict as HM
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
  cmd <- customExecParser (prefs showHelpOnError)
       $ info (helper <*> parser)
              (  fullDesc
              <> header   "Gittery"
              <> progDesc "tool for managing repositories"
              )
  --
  unsetEnv "LANG"
  hostname     <- getHostName
  repositories <- do
    dir  <- getXdgDirectory XdgConfig "gittery"
    cfgs <- listDirectory dir
    fmap concat $ forM cfgs $ \c -> do
      let (nm,ext) = splitExtension c
          isYaml   = ext == ".yml" || ext == ".yaml"
      case isYaml of
        False -> return []
        True  -> Yaml.decodeFileEither (dir </> c) >>= \case
          Left  e -> do putStrLn ("Cannot decode configuration file " ++ c)
                        print e
                        exitFailure
          Right x -> case traverse (HM.lookup (T.pack hostname)) x of
            Nothing -> do putStrLn ("No entry for hostname `"++hostname++"' in "++c)
                          exitFailure
            Just y  -> return [(nm, y)]
  --
  cmd repositories


filterArguments :: [String] -> [(FilePath, a)] -> [(FilePath, a)]
filterArguments []   xs = xs
filterArguments keys xs
  | not (Set.null unknownKeys) = error $ unwords $ "Unknown repo trees:" : Set.toList unknownKeys
  | otherwise                  = filter (\(k,_) -> k `Set.member` keySet) xs
  where
    keySet       = Set.fromList keys
    existingKeys = Set.fromList $ map fst xs
    unknownKeys  = keySet `Set.difference` existingKeys

parser :: Parser ([(FilePath, RepositoryTree FilePath)] -> IO ())
parser = subparser $ mconcat
  [ command "check" $ wrap "Check all repositories"     $ do
      keys <- keyParser
      pure (checkRepositories . filterArguments keys)
  , command "fetch" $ wrap "Fetch for all repositories" $ do
      ctxRepoParams <- ignoreParser
      ctxDryRun     <- dryRunParser
      keys          <- keyParser
      pure $ flip runReaderT Ctx{..} . fetchRepo . filterArguments keys
  , command "push" $ wrap "Try to push all changes to repository" $ do
      ctxRepoParams <- ignoreParser
      ctxDryRun     <- dryRunParser
      keys          <- keyParser
      pure $ flip runReaderT Ctx{..} . pushRepo . filterArguments keys
  , command "init" $ wrap "Create all missing repositories" $ pure cloneRepo
  , command "set-remote" $ wrap "Set all remotes to values from config" $ pure setRemotes
  , command "ls"   $ wrap "List all repository groups"      $ pure lsRepo
   ]
  where
    wrap hlp p   = (helper <*> p) `info` progDesc hlp
    --
    ignoreParser =  many $ asum
      [ IgnoreRemoteName   <$> strOption ( long "ignore-remote-name"
                                        <> help "Ignore remote by its name"
                                         )
      , IgnoreRemotePrefix <$> strOption ( long "ignore-remote-prefix"
                                        <> help "Ignore remote by its name"
                                         )
      , IgnoreRemoteInfix  <$> strOption ( long "ignore-remote-infix"
                                        <> help "Ignore remote by its name"
                                        )
      ]
    dryRunParser = switch ( long "dry-run"
                         <> help "Do nothing"
                          )
    keyParser =  many $ strArgument ( help    "Repo trees to check"
                                   <> metavar "TREE"
                                    )
