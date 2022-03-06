{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Types where

import Data.Generics.Labels ()
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Options.Generic
import Protolude hiding (div, hash, head, link)

--------------------------------------------------------------------------------

newtype Hash = Hash {unHash :: Text}
  deriving newtype (Show, Eq, Ord, FromField, ToField)

newtype DirectoryID = DirectoryID {unUUID :: Text}
  deriving newtype (Show, Eq, Ord, FromField, ToField)

data File = File
  { fileHash :: Hash,
    fileName :: FilePath,
    filePath :: FilePath,
    fileDirectoryID :: DirectoryID
  }
  deriving (Generic, Show)

data Directory = Directory
  { directoryID :: DirectoryID,
    directoryName :: FilePath,
    directoryPath :: FilePath,
    directoryParentID :: Maybe DirectoryID
  }
  deriving (Generic, Show)

data BrowserAction = Preview | Download

data Env = Env
  { rootDir :: FilePath,
    dbConnection :: Connection
  }
  deriving (Generic)

--------------------------------------------------------------------------------

instance FromRow Directory where
  fromRow = Directory <$> field <*> field <*> field <*> field

instance ToRow Directory where
  toRow Directory {..} =
    toRow (directoryID, directoryName, directoryPath, directoryParentID)

instance FromRow File where
  fromRow = File <$> field <*> field <*> field <*> field

instance ToRow File where
  toRow File {..} = toRow (fileHash, fileName, filePath, fileDirectoryID)

--------------------------------------------------------------------------------
