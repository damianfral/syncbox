{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Core where

import BLAKE3 qualified
import Colog
import Data.Text qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Protolude hiding (div, hash, head, link)
import Servant (ServerError)
import Servant.API

--------------------------------------------------------------------------------

hashPath :: FilePath -> Text
hashPath path =
  show (BLAKE3.hash Nothing [encodeUtf8 $ T.pack path] :: BLAKE3.Digest 16)

--------------------------------------------------------------------------------

newtype DirectoryID = DirectoryID {unDirectoryID :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype FileID = FileID {unFileID :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, ToHttpApiData)

directoryIDForPath :: FilePath -> DirectoryID
directoryIDForPath = DirectoryID . hashPath

fileIDForPath :: FilePath -> FileID
fileIDForPath = FileID . hashPath

textFileID :: FileID -> Text
textFileID = unFileID

textDirectoryID :: DirectoryID -> Text
textDirectoryID = unDirectoryID

--------------------------------------------------------------------------------

data File = File
  { fileID :: FileID,
    fileName :: FilePath,
    fileSize :: Integer,
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

--------------------------------------------------------------------------------

instance FromRow Directory

instance ToRow Directory

instance FromRow File

instance ToRow File

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data BrowserAction = Preview | Download

data ViewMode = ListView | GridView
  deriving (Eq, Show)

viewModeText :: ViewMode -> Text
viewModeText ListView = "list"
viewModeText GridView = "grid"

instance FromHttpApiData ViewMode where
  parseQueryParam value = case T.toLower value of
    "list" -> Right ListView
    "grid" -> Right GridView
    _ -> Left "Invalid view mode"

instance ToHttpApiData ViewMode where
  toQueryParam = viewModeText

data Env m = Env
  { rootDir :: FilePath,
    showHidden :: Bool,
    dbConnection :: Connection,
    logAction :: LogAction m Message
  }
  deriving (Generic)

--------------------------------------------------------------------------------

newtype AppM a = AppM {unAppM :: ReaderT (Env AppM) (ExceptT ServerError IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Env AppM),
      MonadIO,
      MonadError ServerError
    )

runAppM :: Env AppM -> AppM a -> IO (Either ServerError a)
runAppM env = runExceptT . flip runReaderT env . unAppM

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = logAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env {logAction = newLogAction}
  {-# INLINE setLogAction #-}
