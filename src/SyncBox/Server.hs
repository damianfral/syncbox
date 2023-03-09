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

import Conduit
import Control.Lens hiding ((<.>))
import Data.Conduit.Process
import Data.Generics.Labels ()
import Data.Text (pack)
import Database.SQLite.Simple
import Network.Mime (defaultMimeLookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Generic
import Protolude hiding (Handler, div, hash, head, link, yield, (<.>))
import Servant.API
import Servant.Conduit ()
import Servant.Server
import Servant.Server.Generic (AsServerT, genericServeT)
import SyncBox.Database
import SyncBox.Render
import SyncBox.Types
import System.Directory (canonicalizePath, makeAbsolute)
import System.Directory.Recursive (getFilesRecursive)
import System.FSNotify as FS
import Text.Blaze.Html (Html)

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

app :: Env -> Application
app env = genericServeT (nt env) server
  where
    nt :: Env -> AppM a -> Handler a
    nt en appM = Handler $ runReaderT appM en

processDirectory :: Directory -> AppM ()
processDirectory dir = mdo
  liftIO $ putStrLn $ "Processing " <> directoryName dir
  files <- liftIO $ getFilesRecursive $ directoryPath dir
  mapM_ insertFile files

processRootDirectory :: AppM ()
processRootDirectory = mdo
  insertRootDirectory >> selectRootDirectory >>= processDirectory

--------------------------------------------------------------------------------

printDB :: Env -> IO ()
printDB Env {..} = do
  directories :: [Directory] <- query_ dbConnection "SELECT * FROM directories;"
  files :: [File] <- query_ dbConnection "SELECT * FROM files;"
  mapM_ print directories
  mapM_ print files

processFSEvent :: FS.Event -> AppM ()
processFSEvent evt = void $ processEvent evt
  where
    processEvent = \case
      (FS.Added filepath _ False) -> insertFile filepath
      (FS.Removed filepath _ False) -> deleteFileByFullPath filepath
      (FS.Added filepath _ True) ->
        insertDirectory filepath >>= processDirectory
      (FS.Removed filepath _ True) -> deleteDirectoryByFullPath filepath
      _ -> pure ()

--------------------------------------------------------------------------------

maybe404orCont :: (a -> AppM Html) -> Maybe a -> AppM Html
maybe404orCont _ Nothing = throwIO err404
maybe404orCont cont (Just v) = cont v

-- response404 :: Response
-- response404 = responseLBS status404 [] ""

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
      let downloadName = pack $ "syncbox-" <> fileName <> ".tar"
      pure $
        addHeader (makeAttachment downloadName) $
          addHeader mimeType $
            compressFile file

makeAttachment :: Text -> Text
makeAttachment fileName = "attachment; filename=" <> fileName

handleFolderPreview :: DirectoryID -> AppM Html
handleFolderPreview uuid = do
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
      let mimeType = decodeUtf8With lenientDecode $ defaultMimeLookup "file.tar"
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
  wd <- liftIO $ canonicalizePath $ directoryPath <> "/.."
  let args = toS <$> ["-cvO", "-C", pack wd, pack directoryPath]
  let cmd = proc "tar" args
  (Inherited, output, Inherited, _) <- streamingProcess cmd
  output

compressFile :: File -> ConduitT Void ByteString IO ()
compressFile File {..} = do
  let wd = "."
  let args = toS <$> ["-cvO", "-C", pack wd, pack filePath]
  let cmd = proc "tar" args
  (Inherited, output, Inherited, _) <- streamingProcess cmd
  output

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
    let env = Env (root opts) db
    putStrLn ("Initializing" :: Text)
    initDB db
    void $ runAppM env processRootDirectory
    putStrLn ("Starting FS Watcher" :: Text)
    withManagerConf defaultConfig {confDebounce = NoDebounce} $ \mgr -> do
      -- start a watching job (in the background)
      void $
        watchTree
          mgr -- manager
          (root opts) -- directory to watch
          (const True) -- predicate
          (void . runAppM env . processFSEvent)
      putStrLn ("Starting server" :: Text)
      run (port opts) . logStdout $ app env
