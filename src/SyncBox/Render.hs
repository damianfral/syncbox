{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Render where

import Control.Arrow ((>>>))
import Data.ByteString.Lazy qualified as BS
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (pack)
import Data.Text qualified as T
import Formatting ((%))
import Formatting qualified as F
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
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes hiding (dir, span)
import Text.Blaze.Html5.Attributes qualified as Attr
import Prelude qualified

------------------------------------------------------------------------------------------

dataView :: AttributeValue -> Attribute
dataView = H.customAttribute "data-view"

renderFile :: ViewMode -> [Directory] -> File -> Html
renderFile view crumbDirs f@File {..} =
  main ! class_ "page" ! dataView (toValue $ viewModeText view) $ do
    section ! class_ "hero" $ do
      nav ! class_ "breadcrumb" ! ariaLabel "Breadcrumb" $
        renderBreadcrumb view crumbDirs (Just $ pack fileName)
      p ! class_ "meta" $ do
        span $ text $ renderFileSize fileSize
        span ! class_ "dot" $ "·"
        span $ text $ fileKind f
    section ! class_ "preview" $ case previewBody f of
      Just previewHtml -> previewHtml
      Nothing -> p ! class_ "empty" $ "No preview available for this file."
    section ! class_ "actions" $ do
      a ! class_ "button" ! href (fileDownloadURI f) $ "Download"

renderBreadcrumb :: ViewMode -> [Directory] -> Maybe Text -> Html
renderBreadcrumb view dirs current = sequence_ $ intersperse sep $ do
  (uncurry renderDir <$> dirItems)
    <> maybe [] (\c -> [span ! class_ "breadcrumb-current" $ text c]) current
  where
    sep = span ! class_ "breadcrumb-sep" $ " / "
    renderDir isLast dir = do
      let labelV = directoryCrumbLabel dir
          hrefDir = directoryPreviewURI view dir
      if isLast && isNothing current
        then span ! class_ "breadcrumb-current" $ text labelV
        else a ! class_ "breadcrumb-link" ! href hrefDir $ text labelV
    dirItems = case dirs of
      [] -> []
      _ ->
        let n = Prelude.length dirs
         in zipWith (\j d -> (j == n, d)) [1 ..] dirs

directoryCrumbLabel :: Directory -> Text
directoryCrumbLabel Directory {..} = case directoryParentID of
  Nothing -> "Home"
  Just _ -> pack directoryName

previewBody :: File -> Maybe Html
previewBody f = case drop 1 $ toLower <$> takeExtension (filePath f) of
  "jpeg" -> Just $ renderImage f
  "jpg" -> Just $ renderImage f
  "png" -> Just $ renderImage f
  "gif" -> Just $ renderImage f
  "webp" -> Just $ renderImage f
  "svg" -> Just $ renderImage f
  "mov" -> Just $ renderVideo f
  "mkv" -> Just $ renderVideo f
  "mp4" -> Just $ renderVideo f
  "webm" -> Just $ renderVideo f
  "avi" -> Just $ renderVideo f
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

renderDirectory ::
  ViewMode -> [Directory] -> Directory -> [(Directory, Integer)] -> [File] -> Html
renderDirectory view crumbDirs Directory {..} subDirs files =
  main ! class_ "page" ! dataView (toValue $ viewModeText view) $ do
    section ! class_ "hero" $ do
      nav ! class_ "breadcrumb" ! ariaLabel "Breadcrumb" $
        renderBreadcrumb view crumbDirs Nothing

    section ! class_ "toolbar" $ do
      p ! class_ "count" $ text countText
      div ! class_ "toolbar-actions" $ do
        viewSwitcher view directoryID
        let hrefV = linkHref $ downloadFolderLink directoryID
        a ! class_ "button secondary" ! href hrefV $ "Download folder"

    if Prelude.null subDirs && Prelude.null files
      then section ! class_ "panel empty-state" $ p ! class_ "empty" $ "Empty"
      else section ! class_ "panel listing" $ do
        div ! class_ "listing-header" $ do
          span "Name"
          span ! class_ "item-size" $ "Size"
          span mempty
        ul ! class_ "listing-body" $ do
          for_ subDirs $ uncurry (renderDirectoryLink view)
          for_ files $ renderFileLink view
  where
    totalItems = Prelude.length subDirs + Prelude.length files
    countText =
      pack (show totalItems) <> if totalItems == 1 then " item" else " items"

ariaLabel :: AttributeValue -> Attribute
ariaLabel = H.customAttribute "aria-label"

ariaCurrent :: AttributeValue -> Attribute
ariaCurrent = H.customAttribute "aria-current"

viewSwitcher :: ViewMode -> DirectoryID -> Html
viewSwitcher view directoryID =
  div ! class_ "segmented" ! role "group" ! ariaLabel "View mode" $ do
    viewButton view directoryID ListView "List"
    viewButton view directoryID GridView "Grid"

viewButton :: ViewMode -> DirectoryID -> ViewMode -> Text -> Html
viewButton current directoryID targetMode labelText = do
  let isActiveClass = if current == targetMode then " is-active" else ""
  a
    ! class_ (toValue @Text $ "view-button" <> isActiveClass)
    ! href (viewHref targetMode $ previewFolderLink directoryID)
    ! ariaCurrent (if current == targetMode then "page" else "false")
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
linkHref l = toValue @Text $ "/" <> show (linkURI l)

viewHref :: ViewMode -> Link -> AttributeValue
viewHref view l =
  toValue @Text $ "/" <> show (linkURI l) <> "?view=" <> viewModeText view

--------------------------------------------------------------------------------

filePreviewURI :: ViewMode -> File -> AttributeValue
filePreviewURI view = fileID >>> previewFileLink >>> viewHref view

fileDownloadURI :: File -> AttributeValue
fileDownloadURI = fileID >>> downloadFileLink >>> linkHref

fileThumbnailURI :: File -> AttributeValue
fileThumbnailURI = fileID >>> fileThumbnail links >>> linkHref

fileRawURI :: File -> AttributeValue
fileRawURI = fileID >>> fileRaw links >>> linkHref

directoryPreviewURI :: ViewMode -> Directory -> AttributeValue
directoryPreviewURI view = directoryID >>> previewFolderLink >>> viewHref view

directoryDownloadURI :: Directory -> AttributeValue
directoryDownloadURI = directoryID >>> downloadFolderLink >>> linkHref

--------------------------------------------------------------------------------
-- List view

renderDirectoryLink :: ViewMode -> Directory -> Integer -> Html
renderDirectoryLink view dir totalSize = li ! class_ "item" $ do
  a ! class_ "item-main" ! href (directoryPreviewURI view dir) $ do
    span ! class_ "badge folder" $ "DIR"
    span ! class_ "item-name" $ text $ pack $ directoryName dir
  span ! class_ "item-size" $ text $ renderFileSize totalSize
  a
    ! class_ "icon-button"
    ! href (directoryDownloadURI dir)
    ! Attr.title "Download folder"
    $ "↓"

renderFileLink :: ViewMode -> File -> Html
renderFileLink view file@File {..} = li ! class_ "item" $ do
  a ! class_ "item-main" ! href (filePreviewURI view file) $ do
    renderFileBadge file
    span ! class_ "item-name" $ text $ pack fileName
  span ! class_ "item-size" $ text $ renderFileSize fileSize
  a
    ! class_ "icon-button"
    ! href (fileDownloadURI file)
    ! Attr.title "Download file"
    $ "↓"

renderFileBadge :: File -> Html
renderFileBadge file = case thumbnailType (filePath file) of
  Just _ -> span ! class_ "thumb" $ img ! src (fileThumbnailURI file) ! alt ""
  Nothing -> span ! class_ "badge" $ text $ fileKind file

--------------------------------------------------------------------------------
-- Cover view

renderVideo :: File -> Html
renderVideo file = do
  let srcV = fileRawURI file
  video ! class_ "media" ! src srcV ! controls "true" ! preload "metadata" $ ""

renderImage :: File -> Html
renderImage file = do
  let altV = toValue $ pack $ fileName file
  img ! class_ "media" ! src (fileRawURI file) ! alt altV

renderTxt :: File -> Html
renderTxt _ = pre ! class_ "text-preview" $ code $ text ""

--------------------------------------------------------------------------------
-- Template

htmlToResponse :: Html -> Response
htmlToResponse page =
  responseLBS status200 [] $ BS.fromStrict $ encodeUtf8 $ pack $ renderHtml $ do
    renderPage page

renderPage :: Html -> Html
renderPage page = docTypeHtml $ do
  head $ do
    meta ! charset "utf-8"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.title "Syncbox"
    H.style $ toHtml styleCss
  body $ do
    header ! class_ "site-header" $ a ! class_ "brand" ! href "/" $ do
      span ! class_ "brand-mark" $ mempty
      span "syncbox"
    page

styleCss :: Text
styleCss = $(makeRelativeToProject "assets/style.css" >>= embedStringFile)
