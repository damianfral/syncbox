{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.API where

import Conduit
import Protolude
import Servant
import Servant.HTML.Blaze
import SyncBox.Types
import qualified Text.Blaze.Html as Blaze

type StreamIOBytes = ConduitT Void ByteString IO ()

type HeadersContent =
  '[Header "Content-Disposition" Text, Header "Content-Type" Text]

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
