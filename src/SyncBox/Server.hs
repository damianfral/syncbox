{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Server where

import Codec.Archive.Tar (write)
import Codec.Archive.Tar.Entry (packFileEntry, toTarPath)
import qualified Codec.Compression.GZip as GZip
import Colog (log)
import Colog.Actions
import Colog.Core
import Conduit
import Control.Lens hiding ((<.>))
import Data.Generics.Labels ()
import Data.Text (pack)
import Database.SQLite.Simple
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
import SyncBox.Types
import System.Directory (makeAbsolute)
import System.Directory.Recursive (getFilesRecursive)
import System.FSNotify as FS
import System.FilePath (makeRelative, takeFileName)
import Text.Blaze.Html (Html)
import qualified Prelude

--------------------------------------------------------------------------------

server :: SyncBoxRoutes (AsServerT AppM)
server =
  SyncBoxRoutes
    { folderPreview = handleFolderPreview,
      folderDownload = handleFolderDownload,
      filePreview = handleFilePreview,
      fileDownload = handleFileDownload,
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
processRootDirectory = do
  dir <- insertRootDirectory
  processDirectory dir

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

handleFilePreview :: FileID -> AppM Html
handleFilePreview fh = do
  mFile <- selectFile fh
  maybe404orCont (pure . renderFile) mFile

handleFileDownload :: FileID -> AppM (WithContent (ConduitT Void ByteString IO ()))
handleFileDownload path =
  selectFile path >>= \case
    Nothing -> throwError err404
    Just file@(File {..}) -> do
      let mimeType = decodeUtf8With lenientDecode $ defaultMimeLookup $ toS filePath
      let downloadName = pack $ "syncbox-" <> fileName <> ".tgz"
      pure $
        addHeader (makeAttachment downloadName) $
          addHeader mimeType $
            compressFile file

makeAttachment :: Text -> Text
makeAttachment fileName = "attachment; filename=\"" <> fileName <> "\""

handleFolderPreview :: DirectoryID -> AppM Html
handleFolderPreview uuid =
  selectDirectory uuid
    >>= maybe404orCont
      ( \dir -> do
          subdDirs <- selectSubDirectories uuid
          files <- selectFilesByDirectory uuid
          pure $ renderDirectory dir subdDirs files
      )

handleFolderDownload ::
  DirectoryID ->
  AppM (WithContent (ConduitT Void ByteString IO ()))
handleFolderDownload uuid = do
  mDir <- selectDirectory uuid
  case mDir of
    Nothing -> throwIO err404
    Just dir@Directory {..} -> do
      let fileName = pack $ "syncbox-" <> directoryName <> ".tar"
      let mimeType = decodeUtf8With lenientDecode $ defaultMimeLookup fileName
      pure $
        addHeader (makeAttachment fileName) $
          addHeader mimeType $
            compressDirectory dir

handleIndex :: AppM Html
handleIndex = do
  root <- selectRootDirectory
  handleFolderPreview $ directoryID root

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
      void $
        liftIO $
          watchTree
            mgr -- manager
            (root opts) -- directory to watch
            (const True) -- predicate
            (void . runAppM env . processFSEvent)
      log I "Starting server"
      liftIO $ run (port opts) . logStdout $ app env
