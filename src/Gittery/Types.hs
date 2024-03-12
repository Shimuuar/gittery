{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
-- |
-- Data types for the 
module Gittery.Types
  ( -- * Config data types
    RepositoryGroup(..)
  , Repository(..)
  , HostInfo(..)
    -- * Reading of configuration
  , readConfig
  , toLocalConfig
  ) where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Text           (Text)
import Data.Traversable
import Data.Yaml           qualified as YAML
import System.Directory
import System.FilePath
import System.Exit
import Network.HostName


----------------------------------------------------------------
-- Readig of configuration files
----------------------------------------------------------------

-- | Read configuration 
readConfig
  :: FilePath
  -> IO (Map String (RepositoryGroup HostInfo))
readConfig dir = do
  files <- listDirectory dir  
  cfgs  <- for files $ \path -> case splitExtension path of
    (nm,ext) | ext == ".yml" || ext == ".yaml" -> do
      YAML.decodeFileEither (dir </> path) >>= \case
        Left  e -> do putStrLn ("Cannot decode configuration file " ++ path)
                      print e
                      exitFailure
        Right x -> pure [(nm, x)]
    _ -> pure []
  pure $ Map.fromList $ concat cfgs

-- | Covert configuration file to local one
toLocalConfig
  :: Map String (RepositoryGroup HostInfo)
  -> IO (Map String (RepositoryGroup FilePath))
toLocalConfig hm = do
  hostname <- getHostName
  fmap (Map.mapMaybe sequence)
    $ Map.traverseWithKey (\k -> traverse (convert hostname k)) hm
  where
    convert hostname k (HostInfo m) = case HM.lookup hostname m of
      Nothing -> do putStrLn $ "No host entry in file: " ++ k
                    exitFailure
      Just x  -> pure x
      

----------------------------------------------------------------
-- Description of configuration files
----------------------------------------------------------------

-- | Description of group of repositories placed in a single directory
data RepositoryGroup a = RepositoryGroup
  { host  :: !a
    -- ^ Information about hosts
  , repos :: !(Map FilePath Repository)
    -- ^ Collection of repositories in the group
  , no_unknown :: !Bool
    -- ^ Whether we should check that directory doesn not contains
    --   unknown repos
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- | Single repository
data Repository = Repository
  { remote :: !(HashMap Text Text)
    -- ^ List of remotes for the repo
  , branches :: ![Text]
  }
  deriving stock (Show, Eq)

-- | How repository is placed on a host
newtype HostInfo = HostInfo 
  { get :: HM.HashMap String (Maybe FilePath) }
  deriving stock   (Show, Eq)
  deriving newtype (FromJSON)

----------------------------------------------------------------
-- Deserialization
----------------------------------------------------------------

instance FromJSON Repository where
  parseJSON = withObject "" $ \o -> do
    remote <- (o .: "remote") >>= \case
      String s -> pure $ HM.singleton "origin" s
      js       -> parseJSON js
    branches <- o .:? "branches" .!= ["master"]
    pure Repository{..}

instance FromJSON a => FromJSON (RepositoryGroup a) where
  parseJSON = withObject "" $ \o -> do
    ("directory"::Text) <- o .: "type"
    host  <- o .: "host"
    repos <- o .: "repos"
    no_unknown <- o .:? "no_unknown" .!= False
    unless (all validPath (Map.keys repos)) $
      fail "Invalid repository name"    
    pure RepositoryGroup{..}
    

validPath :: FilePath -> Bool
validPath []     = False
validPath (s0:s) = ok1 s0 && all ok s
  where
    ok1 c = isAscii c && isAlphaNum c
    ok  c = isAscii c && (isAlphaNum c || c == '-' || c == '_')
