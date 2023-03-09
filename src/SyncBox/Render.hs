{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Render where

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy as BS
import Data.Text (pack)
import qualified Data.Text as T
import Formatting ((%))
import qualified Formatting as F
import Network.HTTP.Types.Status
import Network.Wai
import Protolude hiding (div, hash, head, link, (%))
import Servant (linkURI)
import Servant.API (Link)
import SyncBox.Types
import System.FilePath
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 hiding (style)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (dir)
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Prelude

------------------------------------------------------------------------------------------

renderFile :: File -> Html
renderFile f@File {..} =
  main $ do
    section $
      f & case Prelude.tail $ toLower <$> takeExtension filePath of
        "jpeg" -> renderImage
        "jpg" -> renderImage
        "png" -> renderImage
        "svg" -> renderImage
        "mov" -> renderVideo
        "mkv" -> renderVideo
        "mp4" -> renderVideo
        "txt" -> renderTxt
        "md" -> renderTxt
        "nix" -> renderTxt
        _ -> const mempty
    section ! Attr.style flexColumnCenter $ do
      p $ text $ pack filePath
      a ! Attr.href (fileURI Download f) ! Attr.style "align-self:center" $
        button "Download"

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

renderDirectory :: Directory -> [Directory] -> [File] -> Html
renderDirectory Directory {..} subDirs files =
  main $ do
    section $ h3 $ text $ pack directoryName
    section $ table $ do
      mapM_ (renderDirectoryLink Preview) subDirs
      mapM_ (renderFileLink Preview) files
    section ! Attr.style flexColumnCenter
      $ a
        ! Attr.href (linkHref $ downloadFolderLink directoryID)
        ! Attr.style "align-self:center"
      $ button "Download everything"

--------------------------------------------------------------------------------

previewFolderLink :: DirectoryID -> Link
previewFolderLink = folderPreview links

downloadFolderLink :: DirectoryID -> Link
downloadFolderLink = folderDownload links

previewFileLink :: FileID -> Link
previewFileLink = filePreview links

downloadFileLink :: FileID -> Link
downloadFileLink = fileDownload links

linkHref :: Link -> AttributeValue
linkHref l = toValue ("/" <> show (linkURI l) :: Text)

--------------------------------------------------------------------------------

flexColumnCenter :: AttributeValue
flexColumnCenter = "align-self:center; display:flex; flex-direction:column"

fileURI :: BrowserAction -> File -> AttributeValue
fileURI Preview = fileID >>> previewFileLink >>> linkHref
fileURI Download = fileID >>> downloadFileLink >>> linkHref

directoryURI :: BrowserAction -> Directory -> AttributeValue
directoryURI Preview = directoryID >>> previewFolderLink >>> linkHref
directoryURI Download = directoryID >>> downloadFolderLink >>> linkHref

--------------------------------------------------------------------------------
-- List view

renderDirectoryLink :: BrowserAction -> Directory -> Html
renderDirectoryLink ba dir = tr $ do
  td ! Attr.class_ "w-100 tc" $ text "-"
  td ! Attr.class_ "w-100" $
    a ! Attr.href (directoryURI ba dir) $
      text $
        pack $
          directoryName dir

renderFileLink :: BrowserAction -> File -> Html
renderFileLink ba file@File {..} = tr $ do
  td ! Attr.class_ "w-100 tc" $ text $ renderFileSize fileSize
  td ! Attr.class_ "w-100" $
    a ! Attr.href (fileURI ba file) $
      text $
        pack fileName

--------------------------------------------------------------------------------
-- Cover view

renderVideo :: File -> Html
renderVideo file =
  div $ video ! Attr.src (fileURI Download file) ! Attr.controls "true" $ mempty

renderImage :: File -> Html
renderImage file = div $ img ! src (fileURI Download file)

renderTxt :: File -> Html
renderTxt file = div $ code $ text ""

--------------------------------------------------------------------------------
-- Template

htmlToResponse :: Html -> Response
htmlToResponse page =
  responseLBS status200 [] $
    BS.fromStrict $
      encodeUtf8 $
        pack $
          renderHtml $
            docTypeHtml $ do
              head $ do
                link ! rel "stylesheet" ! href awsm
                link ! rel "stylesheet" ! href tachyons
                H.style $ toHtml $ T.unlines styles
              body page
  where
    styles =
      [ "main{display:flex; flex-direction: column; justify-content: center; height:100%; overflow-y: auto}",
        "body{position:relative;}"
      ]
    awsm = "https://unpkg.com/awsm.css/dist/awsm.min.css"
    tachyons = "https://unpkg.com/tachyons@4.12.0/css/tachyons.min.css"
