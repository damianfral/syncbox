{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SyncBox.Types where

import Conduit
import Control.Applicative
import Data.Generics.Labels ()
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Options.Generic
import Protolude hiding (div, hash, head, link)
import Servant (ServerError)
import Servant.API
import Servant.HTML.Blaze (HTML)
import Servant.Links
import qualified Text.Blaze.Html as Blaze

--------------------------------------------------------------------------------

type StreamIOBytes = ConduitT Void ByteString IO ()

type HeadersContent =
  '[ Header "Content-Disposition" Text,
     Header "Content-Type" Text
   ]

type WithContent a = Headers HeadersContent a

--------------------------------------------------------------------------------

type IndexAPI = Get '[HTML] Blaze.Html

type FolderPreviewAPI =
  "folder"
    :> Capture "nodeID" DirectoryID
    :> "preview"
    :> Get '[HTML] Blaze.Html

type FolderDownloadAPI =
  "folder"
    :> Capture "nodeID" DirectoryID
    :> "download"
    :> StreamGet NoFraming OctetStream (WithContent StreamIOBytes)

type FilePreviewAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "preview"
    :> Get '[HTML] Blaze.Html

type FileDownloadAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "download"
    :> StreamGet NoFraming OctetStream (WithContent StreamIOBytes)

data SyncBoxRoutes context = SyncBoxRoutes
  { indexPage :: context :- IndexAPI,
    folderPreview :: context :- FolderPreviewAPI,
    folderDownload :: context :- FolderDownloadAPI,
    filePreview :: context :- FilePreviewAPI,
    fileDownload :: context :- FileDownloadAPI
  }
  deriving (Generic)

api :: Proxy (ToServantApi SyncBoxRoutes)
api = genericApi (Proxy :: Proxy SyncBoxRoutes)

links :: SyncBoxRoutes (AsLink Link)
links = allFieldLinks

--------------------------------------------------------------------------------

newtype DirectoryID = DirectoryID {unDirectoryID :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, ToHttpApiData)

newtype FileID = FileID {unFileID :: UUID}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromField, ToField, FromHttpApiData, ToHttpApiData)

textFileID :: FileID -> Text
textFileID = UUID.toText . unFileID

textDirectoryID :: DirectoryID -> Text
textDirectoryID = UUID.toText . unDirectoryID

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

instance FromField UUID where
  fromField f = case fieldData f of
    (SQLText txt) -> maybe empty pure $ UUID.fromText txt
    _ -> empty

instance ToField UUID where
  toField = toField . UUID.toText

--------------------------------------------------------------------------------

data BrowserAction = Preview | Download

data Env = Env
  { rootDir :: FilePath,
    dbConnection :: Connection
  }
  deriving (Generic)

--------------------------------------------------------------------------------

type AppM = ReaderT Env (ExceptT ServerError IO)

runAppM :: Env -> AppM a -> IO (Either ServerError a)
runAppM env = runExceptT . flip runReaderT env
