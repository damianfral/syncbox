{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Server where

import Codec.Archive.Tar (write)
import Codec.Archive.Tar.Entry (packFileEntry, toTarPath)
import Codec.Compression.GZip qualified as GZip
import Colog (log)
import Colog.Actions
import Colog.Core
import Conduit
import Control.Lens hiding ((<.>))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.Text (pack)
import Data.Text qualified as T
import Database.SQLite.Simple
import Network.HTTP.Types.Header (hContentLength, hContentRange, hContentType)
import Network.HTTP.Types.Status (status200, status206, status416)
import Network.Mime (defaultMimeLookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Generic
import Protolude hiding (Handler, div, head, link, log)
import Servant.API
import Servant.Conduit ()
import Servant.Server
import Servant.Server.Generic (AsServerT, genericServeT)
import SyncBox.API
import SyncBox.Database
import SyncBox.Render
import SyncBox.Thumbnail
import SyncBox.Types
import System.Directory (makeAbsolute)
import System.Directory.Recursive (getFilesRecursive)
import System.FSNotify as FS
import System.FilePath (makeRelative, takeFileName)
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

processDirectory :: Directory -> AppM ()
processDirectory dir = mdo
  log I $ pack $ "Processing " <> directoryName dir
  files <- liftIO $ getFilesRecursive $ directoryPath dir
  mapM_ insertFile files

processRootDirectory :: AppM ()
processRootDirectory = insertRootDirectory >>= processDirectory

--------------------------------------------------------------------------------

printDB :: Env AppM -> IO ()
printDB Env {..} = do
  directories :: [Directory] <- query_ dbConnection "SELECT * FROM directories;"
  files :: [File] <- query_ dbConnection "SELECT * FROM files;"
  mapM_ print directories
  mapM_ print files

processFSEvent :: FS.Event -> AppM ()
processFSEvent evt = do
  log I $ show evt
  void $ processEvent evt
  where
    processEvent = \case
      (FS.Added filepath _ IsFile) -> insertFile filepath
      (FS.Removed filepath _ IsFile) -> deleteFileByFullPath filepath
      (FS.Added filepath _ IsDirectory) ->
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
  Maybe Text ->
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
      case parseByteRange mRange total of
        Nothing ->
          let headers = (hContentLength, encodeUtf8 $ show total) : commonHeaders
           in respondWai $ responseFile status200 headers filePath Nothing
        Just (Left ()) ->
          let contentRange = "bytes */" <> show total
              headers = [("Accept-Ranges", "bytes"), (hContentRange, contentRange)]
           in respondWai $ responseLBS status416 headers LBS.empty
        Just (Right (start, end)) -> do
          let len = end - start + 1
              range = mconcat ["bytes ", show start, "-", show end, "/", show total]
              headers =
                (hContentLength, encodeUtf8 $ show len)
                  : (hContentRange, range)
                  : commonHeaders
              filePart = FilePart start len total
          respondWai $ responseFile status206 headers filePath (Just filePart)

parseByteRange :: Maybe Text -> Integer -> Maybe (Either () (Integer, Integer))
parseByteRange Nothing _ = Nothing
parseByteRange (Just value) total = do
  range <- T.stripPrefix "bytes=" $ T.strip value
  if T.any (== ',') range
    then Nothing
    else case T.breakOn "-" (T.takeWhile (/= ',') range) of
      ("", suffixText) -> do
        suffix <- readIntegerText $ T.drop 1 suffixText
        if total <= 0 || suffix <= 0
          then Just (Left ())
          else
            let len = min total suffix
             in Just (Right (total - len, total - 1))
      (startText, endPart) -> do
        start <- readIntegerText startText
        let endText = T.drop 1 endPart
        end <-
          if T.null endText then Just (total - 1) else readIntegerText endText
        if total <= 0 || start > end || start >= total
          then Just (Left ())
          else Just (Right (start, min end (total - 1)))

readIntegerText :: Text -> Maybe Integer
readIntegerText t = case reads $ T.unpack t of
  [(n, "")] -> Just n
  _ -> Nothing

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
      let fileName = pack $ "syncbox-" <> directoryName <> ".tar"
      let mimeType = decodeUtf8With lenientDecode $ defaultMimeLookup fileName
      pure $ addHeader (makeAttachment fileName) $ do
        addHeader mimeType $ compressDirectory dir

handleIndex :: Maybe ViewMode -> AppM Html
handleIndex mView =
  selectRootDirectory >>= \root -> handleFolderPreview (directoryID root) mView

resolveView :: Maybe ViewMode -> ViewMode
resolveView = fromMaybe ListView

------------------------------------------------------------------------------------------

compressDirectory :: Directory -> ConduitT Void ByteString IO ()
compressDirectory Directory {..} = do
  fps <- liftIO $ getFilesRecursive directoryPath
  entries <- liftIO $ forM fps $ \fp -> do
    let archivePath = makeRelative directoryPath fp
        tarPath = case toTarPath False archivePath of
          Right tp -> tp
          Left err -> Prelude.error $ "Invalid tar path: " <> err
    packFileEntry fp tarPath
  let tarBS = write entries
  sourceLazy tarBS

compressFile :: File -> ConduitT Void ByteString IO ()
compressFile File {..} = do
  let archivePath = takeFileName filePath
      tarPath = case toTarPath False archivePath of
        Right tp -> tp
        Left err -> Prelude.error $ "Invalid tar path: " <> err
  entry <- liftIO $ packFileEntry filePath tarPath
  let tarBS = write [entry]
  sourceLazy $ GZip.compress tarBS

------------------------------------------------------------------------------------------

data ServerOptions = ServerOptions
  { port :: Int,
    root :: FilePath
  }
  deriving (Show, Generic)

instance ParseRecord ServerOptions

--------------------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  opts' :: ServerOptions <- getRecord "syncbox"
  opts <- do
    absPath <- makeAbsolute $ root opts'
    pure $ opts' & #root .~ absPath
  withConnection ":memory:" $ \db -> do
    let env = Env (root opts) db richMessageAction
    initDB db
    void $ runAppM env processRootDirectory
    withManager $ \mgr -> void $ runAppM env $ watcher opts mgr env
  where
    watcher :: ServerOptions -> WatchManager -> Env AppM -> AppM ()
    watcher opts mgr env = do
      log I "Starting FS Watcher"
      let dir = root opts
      let actionPredicate = const True
      let runApp = void . runAppM env . processFSEvent
      void $ liftIO $ watchTree mgr dir actionPredicate runApp
      log I "Starting server"
      liftIO $ run (port opts) . logStdout $ app env
