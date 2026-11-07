{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Render where

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy as BS
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (pack)
import qualified Data.Text as T
import Formatting ((%))
import qualified Formatting as F
import Network.HTTP.Types.Status
import Network.Wai
import Protolude hiding (div, hash, head, link, (%))
import Servant (linkURI)
import Servant.API (Link)
import SyncBox.API
import SyncBox.Thumbnail (thumbnailType)
import SyncBox.Types
import System.FilePath
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 hiding (style)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (dir, span)
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Prelude

------------------------------------------------------------------------------------------

renderFile :: ViewMode -> File -> Html
renderFile view f@File {..} =
  main ! Attr.class_ "page" $ do
    section ! Attr.class_ "hero" $ do
      a
        ! Attr.class_ "back"
        ! Attr.href (viewHref view $ previewFolderLink fileDirectoryID)
        $ "← Back to folder"
      h1 ! Attr.class_ "title" $ text $ pack fileName
      p ! Attr.class_ "meta" $ do
        span $ text $ renderFileSize fileSize
        span ! Attr.class_ "dot" $ "·"
        span $ text $ fileKind f
      p ! Attr.class_ "path" $ text $ pack filePath
    section ! Attr.class_ "preview" $ case previewBody f of
      Just previewHtml -> previewHtml
      Nothing -> p ! Attr.class_ "empty" $ "No preview available for this file."
    section ! Attr.class_ "actions" $
      a ! Attr.class_ "button" ! Attr.href (fileDownloadURI f) $
        "Download"

previewBody :: File -> Maybe Html
previewBody f = case drop 1 $ toLower <$> takeExtension (filePath f) of
  "jpeg" -> Just $ renderImage f
  "jpg" -> Just $ renderImage f
  "png" -> Just $ renderImage f
  "svg" -> Just $ renderImage f
  "mov" -> Just $ renderVideo f
  "mkv" -> Just $ renderVideo f
  "mp4" -> Just $ renderVideo f
  "txt" -> Just $ renderTxt f
  "md" -> Just $ renderTxt f
  "nix" -> Just $ renderTxt f
  _ -> Nothing

fileKind :: File -> Text
fileKind File {..} =
  let extension = drop 1 $ toLower <$> takeExtension filePath
   in if Prelude.null extension then "File" else T.toUpper $ pack extension

renderFileSize :: Integer -> Text
renderFileSize x
  | x < baseI 1 = F.sformat (F.int % " B") x
  | x < baseI 2 = F.sformat (F.int % " KB") (x `Prelude.div` baseI 1)
  | x < baseI 3 = F.sformat (F.fixed 1 % " MB") (y / baseR 2)
  | x < baseI 4 = F.sformat (F.fixed 1 % " GB") (y / baseR 3)
  | otherwise = F.sformat (F.fixed 1 % " TB") (y / baseR 4)
  where
    baseI :: Integer -> Integer
    baseI n = (1024 :: Integer) ^ n
    baseR :: Double -> Double
    baseR n = 1024 ** n
    y = fromInteger x

renderDirectory :: ViewMode -> Directory -> [Directory] -> [File] -> Html
renderDirectory view Directory {..} subDirs files =
  main
    ! Attr.class_ "page"
    ! H.customAttribute "data-view" (toValue $ viewModeText view)
    $ do
      section ! Attr.class_ "hero" $ do
        case directoryParentID of
          Just parentID ->
            a
              ! Attr.class_ "back"
              ! Attr.href (viewHref view $ previewFolderLink parentID)
              $ "← Back"
          Nothing -> mempty
        p ! Attr.class_ "eyebrow" $ "Directory"
        h1 ! Attr.class_ "title" $ text $ pack directoryName
        p ! Attr.class_ "path" $ text $ pack directoryPath
      section ! Attr.class_ "toolbar" $ do
        p ! Attr.class_ "count" $
          text $
            pack (show $ Prelude.length subDirs + Prelude.length files)
              <> if Prelude.length subDirs + Prelude.length files == 1
                then " item"
                else " items"
        div ! Attr.class_ "toolbar-actions" $ do
          viewSwitcher view directoryID
          a
            ! Attr.class_ "button secondary"
            ! Attr.href (linkHref $ downloadFolderLink directoryID)
            $ "Download folder"
      if Prelude.null subDirs && Prelude.null files
        then section ! Attr.class_ "panel empty-state" $ do
          p ! Attr.class_ "empty" $ "This folder is empty."
        else section ! Attr.class_ "panel listing" $ do
          div ! Attr.class_ "listing-header" $ do
            span "Name"
            span ! Attr.class_ "item-size" $ "Size"
            span mempty
          ul ! Attr.class_ "listing-body" $ do
            mapM_ (renderDirectoryLink view) subDirs
            mapM_ (renderFileLink view) files

viewSwitcher :: ViewMode -> DirectoryID -> Html
viewSwitcher view directoryID =
  div
    ! Attr.class_ "segmented"
    ! Attr.role "group"
    ! H.customAttribute "aria-label" "View mode"
    $ do
      viewButton view directoryID ListView "List"
      viewButton view directoryID GridView "Grid"

viewButton :: ViewMode -> DirectoryID -> ViewMode -> Text -> Html
viewButton current directoryID targetMode labelText =
  a
    ! Attr.class_ (toValue (("view-button" <> if current == targetMode then " is-active" else "") :: Text))
    ! Attr.href (viewHref targetMode $ previewFolderLink directoryID)
    ! H.customAttribute "aria-current" (if current == targetMode then "page" else "false")
    $ text labelText

--------------------------------------------------------------------------------

previewFolderLink :: DirectoryID -> Link
previewFolderLink directoryID' = folderPreview links directoryID' Nothing

downloadFolderLink :: DirectoryID -> Link
downloadFolderLink = folderDownload links

previewFileLink :: FileID -> Link
previewFileLink fileID' = filePreview links fileID' Nothing

downloadFileLink :: FileID -> Link
downloadFileLink = fileDownload links

linkHref :: Link -> AttributeValue
linkHref l = toValue ("/" <> show (linkURI l) :: Text)

viewHref :: ViewMode -> Link -> AttributeValue
viewHref view l =
  toValue ("/" <> show (linkURI l) <> "?view=" <> viewModeText view :: Text)

--------------------------------------------------------------------------------

filePreviewURI :: ViewMode -> File -> AttributeValue
filePreviewURI view = fileID >>> previewFileLink >>> viewHref view

fileDownloadURI :: File -> AttributeValue
fileDownloadURI = fileID >>> downloadFileLink >>> linkHref

fileThumbnailURI :: File -> AttributeValue
fileThumbnailURI = fileID >>> fileThumbnail links >>> linkHref

directoryPreviewURI :: ViewMode -> Directory -> AttributeValue
directoryPreviewURI view = directoryID >>> previewFolderLink >>> viewHref view

directoryDownloadURI :: Directory -> AttributeValue
directoryDownloadURI = directoryID >>> downloadFolderLink >>> linkHref

--------------------------------------------------------------------------------
-- List view

renderDirectoryLink :: ViewMode -> Directory -> Html
renderDirectoryLink view dir = li ! Attr.class_ "item" $ do
  a ! Attr.class_ "item-main" ! Attr.href (directoryPreviewURI view dir) $ do
    span ! Attr.class_ "badge folder" $ "DIR"
    span ! Attr.class_ "item-name" $ text $ pack $ directoryName dir
  span ! Attr.class_ "item-size" $ "—"
  a
    ! Attr.class_ "icon-button"
    ! Attr.href (directoryDownloadURI dir)
    ! Attr.title "Download folder"
    $ "↓"

renderFileLink :: ViewMode -> File -> Html
renderFileLink view file@File {..} = li ! Attr.class_ "item" $ do
  a ! Attr.class_ "item-main" ! Attr.href (filePreviewURI view file) $ do
    renderFileBadge file
    span ! Attr.class_ "item-name" $ text $ pack fileName
  span ! Attr.class_ "item-size" $ text $ renderFileSize fileSize
  a
    ! Attr.class_ "icon-button"
    ! Attr.href (fileDownloadURI file)
    ! Attr.title "Download file"
    $ "↓"

renderFileBadge :: File -> Html
renderFileBadge file =
  case thumbnailType (filePath file) of
    Just _ ->
      span ! Attr.class_ "thumb" $
        img
          ! Attr.src (fileThumbnailURI file)
          ! Attr.alt ""
    Nothing -> span ! Attr.class_ "badge" $ text $ fileKind file

--------------------------------------------------------------------------------
-- Cover view

renderVideo :: File -> Html
renderVideo file =
  video
    ! Attr.class_ "media"
    ! Attr.src (fileDownloadURI file)
    ! Attr.controls "true"
    ! Attr.preload "metadata"
    $ mempty

renderImage :: File -> Html
renderImage file =
  img
    ! Attr.class_ "media"
    ! src (fileDownloadURI file)
    ! alt (toValue $ pack $ fileName file)

renderTxt :: File -> Html
renderTxt _ = pre ! Attr.class_ "text-preview" $ code $ text ""

--------------------------------------------------------------------------------
-- Template

htmlToResponse :: Html -> Response
htmlToResponse page =
  responseLBS status200 [] $ BS.fromStrict $ encodeUtf8 $ pack $ renderHtml $ do
    renderPage page

renderPage :: Html -> Html
renderPage page =
  docTypeHtml $ do
    head $ do
      meta ! Attr.charset "utf-8"
      meta ! Attr.name "viewport" ! Attr.content "width=device-width, initial-scale=1"
      H.title "Syncbox"
      H.style $ toHtml styleCss
    body $ do
      header ! Attr.class_ "site-header" $
        a ! Attr.class_ "brand" ! Attr.href "/" $ do
          span ! Attr.class_ "brand-mark" $ mempty
          span "syncbox"
      page

styleCss :: Prelude.String
styleCss = $(makeRelativeToProject "assets/style.css" >>= embedStringFile)
