{-# LANGUAGE OverloadedStrings #-}
-- |
-- Git interaction
module Gittery.Git
  ( -- * Low level
    GitException(..)
  , readGitOutput
    -- * High level
  , gitIsSameRevision
  , gitIsAncestor
  , gitRemotes
  , gitCurrentBranch
  , gitUncommited
  ) where

import Control.Exception
import Data.Char                (isSpace)
import Data.Text                qualified as T
import Data.Text.Lazy           qualified as TL
import Data.Text                (Text)
import Data.Text.Lazy.Encoding  qualified as TL
import Data.Text.Encoding.Error qualified as T
import Data.HashMap.Strict      qualified as HM
import System.Process.Typed


----------------------------------------------------------------
-- Low level commands
----------------------------------------------------------------

-- | Exception during git command execution
newtype GitException = GitException [String]
  deriving stock    Show
  deriving anyclass Exception

-- | Read git output
readGitOutput :: [String] -> IO String
readGitOutput args = do
  (code,out,err) <- readProcess $ proc "git" args
  case code of
    ExitSuccess   -> pure $ decoder out
    ExitFailure n -> throwIO $ GitException
                   $ ("Git exited with error code " ++ show n)
                   : lines (decoder err)
  where
    decoder = TL.unpack . TL.decodeUtf8With T.strictDecode


----------------------------------------------------------------
-- Higher level functions
----------------------------------------------------------------

-- | Check if two branches reference sme revision
gitIsSameRevision :: String -> String -> IO Bool
gitIsSameRevision ref1 ref2 = do
  -- FIXME: We should not fail when reference does not exists!
  hash1 <- readGitOutput ["rev-parse", ref1, "--"]
  hash2 <- readGitOutput ["rev-parse", ref2, "--"]
  return $! hash1 == hash2

-- | Check if ref1 is ancestor of ref2
gitIsAncestor :: String -> String -> IO Bool
gitIsAncestor ref1 ref2 =
  -- FIXME: We should not fail when reference does not exists!
  runProcess (proc "git" ["merge-base", "--is-ancestor", ref1, ref2]) >>= \case
    ExitSuccess   -> pure True
    ExitFailure 1 -> pure False
    ExitFailure i -> error ("ExitFailure: " ++ show i)

-- | Get name of current branch
gitCurrentBranch :: IO String
gitCurrentBranch
  =  reverse . dropWhile (\c -> c=='\n' || c=='\r') . reverse
 <$> readGitOutput ["rev-parse", "--abbrev-ref", "HEAD"]

-- | List git remotes
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

-- | List uncommited changes ignoring untracked files
gitUncommited :: IO [String]
gitUncommited = lines <$> readGitOutput ["status", "-s", "-uno"]
