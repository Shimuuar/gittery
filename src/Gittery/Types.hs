{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module Gittery.Types where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

----------------------------------------------------------------

-- | Description of tree of repositories
data RepositoryTree a = RepositoryTree
  { treeLocation :: !a
  , treeRepos    :: !(HashMap Text Repository)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Single repository
data Repository = Repository
  { repoType :: !RepoType
  , remote   :: !Remote
  }
  deriving (Show, Eq)

data Remote
  = RemoteSimple !Text
  | RemoteMany   !(HashMap Text Text)
  deriving (Show, Eq)

data RepoType
  = GIT
  | HG
  deriving (Show, Eq)

instance FromJSON RepoType where
  parseJSON = withText "" $ \s -> case T.toLower s of
    "hg"  -> pure HG
    "git" -> pure GIT
    _     -> fail $ "Unknown repository type: " ++ T.unpack s

instance FromJSON Remote where
  parseJSON (String s) = pure $ RemoteSimple s
  parseJSON (Object o) = do
    unless (all validStr (HM.keys o)) $
      fail "Invalid remote name"
    RemoteMany <$> traverse parseJSON o
  parseJSON _          = fail "Remote: expecting string or dictionary"

instance FromJSON Repository where
  parseJSON = withObject "" $ \o -> do
    ty <- (o .: "type") <|> pure GIT
    r  <- o .: "remote"
    pure $ Repository ty r

instance FromJSON a => FromJSON (RepositoryTree a) where
  parseJSON = withObject "" $ \o -> do
    ("directory"::Text) <- o .: "type"
    loc <- o .: "location"
    rep <- o .: "repos"
    unless (all validStr (HM.keys rep)) $
      fail "Invalid repository name"    
    pure $ RepositoryTree loc rep
    

validStr :: Text -> Bool
validStr s = case T.uncons s of
  Nothing     -> False
  Just (c,s') -> ok1 c && T.all ok s'
  where
    ok1 c = isAscii c && isAlphaNum c
    ok  c = isAscii c && (isAlphaNum c || c == '-' || c == '_')
----------------------------------------------------------------
