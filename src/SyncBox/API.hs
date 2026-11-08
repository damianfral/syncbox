{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.API where

import Conduit
import Protolude
import Servant
import Servant.HTML.Blaze
import SyncBox.Core
import Text.Blaze.Html5 (Html)

type StreamIOBytes = ConduitT Void ByteString IO ()

type HeadersContent =
  '[Header "Content-Disposition" Text, Header "Content-Type" Text]

type WithContent a = Headers HeadersContent a

--------------------------------------------------------------------------------

type IndexAPI = QueryParam "view" ViewMode :> Get '[HTML] Html

type FolderPreviewAPI =
  "folder"
    :> Capture "nodeID" DirectoryID
    :> "preview"
    :> QueryParam "view" ViewMode
    :> Get '[HTML] Html

type FolderDownloadAPI =
  "folder"
    :> Capture "nodeID" DirectoryID
    :> "download"
    :> StreamGet NoFraming OctetStream (WithContent StreamIOBytes)

type FilePreviewAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "preview"
    :> QueryParam "view" ViewMode
    :> Get '[HTML] Html

type FileDownloadAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "download"
    :> StreamGet NoFraming OctetStream (WithContent StreamIOBytes)

type FileThumbnailAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "thumbnail"
    :> Get '[OctetStream] (Headers '[Header "Content-Type" Text] ByteString)

type FileRawAPI =
  "file"
    :> Capture "nodeID" FileID
    :> "raw"
    :> Header "Range" Range
    :> RawM

data SyncBoxRoutes context = SyncBoxRoutes
  { indexPage :: context :- IndexAPI,
    folderPreview :: context :- FolderPreviewAPI,
    folderDownload :: context :- FolderDownloadAPI,
    filePreview :: context :- FilePreviewAPI,
    fileDownload :: context :- FileDownloadAPI,
    fileThumbnail :: context :- FileThumbnailAPI,
    fileRaw :: context :- FileRawAPI
  }
  deriving (Generic)

api :: Proxy (ToServantApi SyncBoxRoutes)
api = genericApi (Proxy :: Proxy SyncBoxRoutes)

links :: SyncBoxRoutes (AsLink Link)
links = allFieldLinks
