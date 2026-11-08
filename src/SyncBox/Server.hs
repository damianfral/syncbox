{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Server where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry (packFileEntry, toTarPath)
import Codec.Compression.GZip qualified as GZip
import Colog (log)
import Colog.Core
import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (pack)
import Network.HTTP.Types.Header (hContentLength, hContentRange, hContentType)
import Network.HTTP.Types.Status (status200, status206, status416)
import Network.Mime (defaultMimeLookup)
import Network.Wai
import Options.Generic
import Protolude hiding (Handler, div, head, link, log)
import Servant.API
import Servant.Conduit ()
import Servant.Server
import Servant.Server.Generic (AsServerT, genericServeT)
import SyncBox.API
import SyncBox.Core
import SyncBox.Database
import SyncBox.Render
import SyncBox.Thumbnail
import System.Directory.Recursive (getFilesRecursive)
import System.FSNotify as FS
import System.FilePath (makeRelative, splitDirectories, takeFileName)
import Text.Blaze.Html (Html)
import Prelude qualified

--------------------------------------------------------------------------------

server :: SyncBoxRoutes (AsServerT AppM)
server =
  SyncBoxRoutes
    { folderPreview = handleFolderPreview,
      folderDownload = handleFolderDownload,
      filePreview = handleFilePreview,
      fileDownload = handleFileDownload,
      fileThumbnail = handleFileThumbnail,
      fileRaw = handleFileRaw,
      indexPage = handleIndex
    }

app :: Env AppM -> Application
app env = genericServeT (nt env) server
  where
    nt :: Env AppM -> AppM a -> Handler a
    nt en appM = Handler $ runReaderT (unAppM appM) en

isHiddenPath :: FilePath -> Bool
isHiddenPath = any isDot . splitDirectories
  where
    isDot ('.' : _) = True
    isDot _ = False

shouldIncludePath :: Bool -> FilePath -> Bool
shouldIncludePath showHidden path = showHidden || not (isHiddenPath path)

processDirectory :: Directory -> AppM ()
processDirectory dir = do
  Env {..} <- ask
  log I $ pack $ "Processing " <> directoryName dir
  files <- liftIO $ getFilesRecursive $ directoryPath dir
  let visibleFiles = filter (shouldIncludePath showHidden) files
  mapM_ insertFile visibleFiles

processRootDirectory :: AppM ()
processRootDirectory = insertRootDirectory >>= processDirectory

--------------------------------------------------------------------------------

processFSEvent :: FS.Event -> AppM ()
processFSEvent evt = do
  Env {..} <- ask
  log I $ show evt
  void $ processEvent showHidden evt
  where
    processEvent showHidden = \case
      (FS.Added filepath _ IsFile)
        | shouldIncludePath showHidden filepath -> insertFile filepath
      (FS.Removed filepath _ IsFile) -> deleteFileByFullPath filepath
      (FS.Added filepath _ IsDirectory)
        | shouldIncludePath showHidden filepath ->
            insertDirectory filepath >>= processDirectory
      (FS.Removed filepath _ IsDirectory) -> deleteDirectoryByFullPath filepath
      _ -> pure ()

--------------------------------------------------------------------------------

maybe404orCont :: (a -> AppM Html) -> Maybe a -> AppM Html
maybe404orCont _ Nothing = throwIO err404
maybe404orCont cont (Just v) = cont v

handleFilePreview :: FileID -> Maybe ViewMode -> AppM Html
handleFilePreview fh mView = do
  mFile <- selectFile fh
  let viewV = resolveView mView
  maybe404orCont (renderFilePage viewV) mFile
  where
    renderFilePage viewV file = do
      mDir <- selectDirectory (fileDirectoryID file)
      crumbDirs <- case mDir of
        Nothing -> pure []
        Just dir -> selectDirectoryAncestors dir
      pure $ renderPage $ renderFile viewV crumbDirs file

handleFileThumbnail ::
  FileID ->
  AppM (Headers '[Header "Content-Type" Text] ByteString)
handleFileThumbnail fileID = do
  selectFile fileID >>= \case
    Nothing -> throwIO err404
    Just file -> do
      thumbnail <- liftIO $ ensureThumbnail file
      case thumbnail of
        Left err -> do
          log W $ "Thumbnail unavailable for " <> pack (filePath file) <> ": " <> err
          throwIO err404
        Right path -> do
          contentResult <- liftIO $ try @IOException $ BS.readFile path
          case contentResult of
            Left readErr -> do
              log W $
                "Thumbnail read failed for "
                  <> pack (filePath file)
                  <> ": "
                  <> pack (displayException readErr)
              throwIO err404
            Right content -> pure $ addHeader "image/jpeg" content

handleFileRaw ::
  FileID ->
  Maybe Range ->
  Request ->
  (Response -> IO ResponseReceived) ->
  AppM ResponseReceived
handleFileRaw fID mRange _req respondWai =
  selectFile fID >>= \case
    Nothing -> throwError err404
    Just File {..} -> liftIO $ do
      let mimeType = defaultMimeLookup $ toS filePath
          total = fileSize
          commonHeaders = [("Accept-Ranges", "bytes"), (hContentType, mimeType)]
      case mRange of
        Nothing ->
          let headers = (hContentLength, encodeUtf8 $ show total) : commonHeaders
           in respondWai $ responseFile status200 headers filePath Nothing
        Just range -> case resolveRange range total of
          Nothing ->
            let contentRange = "bytes */" <> show total
                headers = [("Accept-Ranges", "bytes"), (hContentRange, contentRange)]
             in respondWai $ responseLBS status416 headers LBS.empty
          Just (Range start end) -> do
            let len = end - start + 1
                range' = mconcat ["bytes ", show start, "-", show end, "/", show total]
                headers =
                  (hContentLength, encodeUtf8 $ show len)
                    : (hContentRange, range')
                    : commonHeaders
                filePart = FilePart start len total
            respondWai $ responseFile status206 headers filePath (Just filePart)

-- | RFC 9110
resolveRange :: Range -> Integer -> Maybe Range
resolveRange (Range start end) total
  -- suffix: last ened bytes
  | start == -1 = do
      guard $ total > 0 && end > 0
      let len = min total end in Just $ Range (total - len) (total - 1)
  -- open end
  | end == -1 = do
      guard $ total > 0 && start >= 0 && start < total
      let end' = total - 1 in Just $ Range start end'
  -- closed: start-eend
  | otherwise = do
      guard $ total > 0 && start >= 0 && start <= end && start < total
      Just $ Range start $ min end (total - 1)

handleFileDownload :: FileID -> AppM (WithContent (ConduitT Void ByteString IO ()))
handleFileDownload path =
  selectFile path >>= \case
    Nothing -> throwError err404
    Just file@(File {..}) -> do
      let mimeType = defaultMimeLookup $ toS filePath
      let downloadName = pack $ "syncbox-" <> fileName <> ".tgz"
      pure $ addHeader (makeAttachment downloadName) $ do
        addHeader (decodeUtf8With lenientDecode mimeType) $ compressFile file

makeAttachment :: Text -> Text
makeAttachment fileName = "attachment; filename=\"" <> fileName <> "\""

handleFolderPreview :: DirectoryID -> Maybe ViewMode -> AppM Html
handleFolderPreview uuid mView =
  selectDirectory uuid >>= maybe404orCont getDirAndRender
  where
    viewMode = resolveView mView
    getDirAndRender dir = do
      crumbDirs <- selectDirectoryAncestors dir
      subdDirs <- selectSubDirectories uuid
      subdDirsWithSize <- for subdDirs $ \subDir -> do
        totalSize <- selectDirectorySize (directoryID subDir)
        pure (subDir, totalSize)
      files <- selectFilesByDirectory uuid
      pure $ renderPage $ renderDirectory viewMode crumbDirs dir subdDirsWithSize files

handleFolderDownload ::
  DirectoryID -> AppM (WithContent (ConduitT Void ByteString IO ()))
handleFolderDownload uuid =
  selectDirectory uuid >>= \case
    Nothing -> throwIO err404
    Just dir@Directory {..} -> do
      Env {..} <- ask
      let fileName = pack $ "syncbox-" <> directoryName <> ".tgz"
      let mimeType = decodeUtf8With lenientDecode $ defaultMimeLookup fileName
      pure $ addHeader (makeAttachment fileName) $ do
        addHeader mimeType $ compressDirectory showHidden dir

handleIndex :: Maybe ViewMode -> AppM Html
handleIndex mView =
  selectRootDirectory >>= \root -> handleFolderPreview (directoryID root) mView

resolveView :: Maybe ViewMode -> ViewMode
resolveView = fromMaybe ListView

------------------------------------------------------------------------------------------

compressDirectory :: Bool -> Directory -> ConduitT Void ByteString IO ()
compressDirectory showHidden Directory {..} = do
  fps <- liftIO $ getFilesRecursive directoryPath
  let archivePaths =
        [ makeRelative directoryPath fp
        | fp <- fps,
          shouldIncludePath showHidden (makeRelative directoryPath fp)
        ]
  entries <- liftIO $ Tar.pack directoryPath archivePaths
  sourceLazy $ GZip.compress $ Tar.write entries

compressFile :: File -> ConduitT Void ByteString IO ()
compressFile File {..} = do
  let archivePath = takeFileName filePath
      tarPath = case toTarPath False archivePath of
        Right tp -> tp
        Left err -> Prelude.error $ "Invalid tar path: " <> err
  entry <- liftIO $ packFileEntry filePath tarPath
  sourceLazy $ GZip.compress $ Tar.write [entry]
