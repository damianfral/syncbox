{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Render where

import SyncBox.Types
import Control.Lens
import qualified Data.ByteString.Lazy as BS
import Data.Generics.Labels ()
import Data.Text (pack)
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Network.Wai
import Protolude hiding (div, hash, head, link)
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
        _ -> const mempty
    section ! Attr.style flexColumnCenter $ do
      p $ text $ pack filePath
      a ! Attr.href (fileURI Download f) ! Attr.style "align-self:center" $
        button "Download"

renderDirectory :: Directory -> [Directory] -> [File] -> Html
renderDirectory dir@Directory {..} subDirs files =
  main $ do
    section $ h3 $ text $ pack directoryName
    section $ do
      mapM_ (p . renderDirectoryLink Preview) subDirs
      mapM_ (p . renderFileLink Preview) files
    section ! Attr.style flexColumnCenter $ do
      a ! Attr.href (directoryURI Download dir) ! Attr.style "align-self:center" $
        button "Download everything"

flexColumnCenter :: AttributeValue
flexColumnCenter = "align-self:center; display:flex; flex-direction:column"

fileURI :: BrowserAction -> File -> AttributeValue
fileURI ba File {..} =
  toValue $ T.unpack ("/file/" <> unHash fileHash) <> queryParams ba

directoryURI :: BrowserAction -> Directory -> AttributeValue
directoryURI ba Directory {..} =
  toValue $ toS ("/folder/" <> unUUID directoryID) <> queryParams ba

queryParams :: BrowserAction -> FilePath
queryParams Preview = "?preview"
queryParams Download = "?download"

renderDirectoryLink :: BrowserAction -> Directory -> Html
renderDirectoryLink ba dir =
  a ! Attr.href (directoryURI ba dir) $ text $ pack $ directoryName dir

renderFileLink :: BrowserAction -> File -> Html
renderFileLink ba file =
  a ! Attr.href (fileURI ba file) $ text $ pack $ fileName file

renderVideo :: File -> Html
renderVideo file =
  div $ video ! Attr.src (fileURI Download file) ! Attr.controls "true" $ mempty

renderImage :: File -> Html
renderImage file = div $ img ! src (fileURI Download file)

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
                H.style $ toHtml $ T.unlines styles
              body page
  where
    styles =
      [ "main{display:flex; flex-direction: column; justify-content: center; height:100%;}",
        "body{height:calc(100vh - 2.8rem); position:relative}"
      ]
    awsm = "https://unpkg.com/awsm.css/dist/awsm.min.css"
