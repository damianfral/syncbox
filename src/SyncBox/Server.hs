{-# LANGUAGE BlockArguments #-}
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
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.ByteString (singleton)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List as Conduit
import Data.Generics.Labels ()
import Database.SQLite.Simple
import Network.HTTP.Types.Status
import Network.Mime (defaultMimeLookup)
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Generic
import Protolude hiding (div, hash, head, link, yield, (<.>))
import qualified Streamly.Prelude as S
import qualified Streamly.System.Process as Process
import SyncBox.Database
import SyncBox.Render
import SyncBox.Types
import System.Directory (makeAbsolute)
import System.Directory.Recursive (getFilesRecursive)
import System.FSNotify as FS
import System.FilePath

--------------------------------------------------------------------------------

processDirectory :: Env -> Directory -> IO ()
processDirectory env dir = mdo
  files <- getFilesRecursive $ directoryPath dir
  mapM_ (insertFile env) files

processRootDirectory :: Env -> IO ()
processRootDirectory env = mdo
  void $ insertRootDirectory env
  selectRootDirectory env >>= processDirectory env

--------------------------------------------------------------------------------

printDB :: Env -> IO ()
printDB Env {..} = do
  directories :: [Directory] <- query_ dbConnection "SELECT * FROM directories;"
  files :: [File] <- query_ dbConnection "SELECT * FROM files;"
  mapM_ print directories
  mapM_ print files

processFSEvent :: Env -> FS.Event -> IO ()
processFSEvent env evt = void $ processEvent evt
  where
    processEvent = \case
      (FS.Added filepath _ False) -> insertFile env filepath
      (FS.Removed filepath _ False) -> deleteFileByFullPath env filepath
      (FS.Modified filepath _ False) -> updateFileByFullPath env filepath
      (FS.Added filepath _ True) ->
        insertDirectory env filepath >>= processDirectory env
      (FS.Removed filepath _ True) -> deleteDirectoryByFullPath env filepath
      _ -> pure ()

--------------------------------------------------------------------------------

parseActionFromRequest :: Request -> BrowserAction
parseActionFromRequest request = queryToAction
  where
    queryItems = queryString request
    queryToAction = case queryItems of
      [("preview", _)] -> Preview
      _ -> Download

makeApplication :: Env -> Application
makeApplication env request respond = do
  response <-
    case (pathInfo request, parseActionFromRequest request) of
      ([], _) -> handleIndex env
      (["file", fh], Preview) -> handleFilePreview env $ Hash fh
      (["file", fh], Download) -> handleFileDownload env $ Hash fh
      (["folder", uuid], Preview) ->
        handleFolderPreview env $ DirectoryID uuid
      (["folder", uuid], Download) -> runResourceT $ do
        handleFolderDownload env $ DirectoryID uuid
      _ -> pure response404
  respond response

maybe404orCont :: Applicative f => (a -> f Response) -> Maybe a -> f Response
maybe404orCont _ Nothing = pure response404
maybe404orCont cont (Just v) = cont v

response404 :: Response
response404 = responseLBS status404 [] ""

handleFilePreview :: Env -> Hash -> IO Response
handleFilePreview env fh = do
  mFile <- selectFile env fh
  maybe404orCont (pure . htmlToResponse . renderFile) mFile

handleFileDownload :: Env -> Hash -> IO Response
handleFileDownload env@Env {..} path =
  selectFile env path >>= \case
    Nothing -> pure $ responseLBS status404 [] ""
    Just File {..} -> do
      let mimeType = defaultMimeLookup $ toS filePath
      let respHeaders =
            [ ("Content-Type", mimeType),
              ( "Content-Disposition",
                "attachment; filename="
                  <> encodeUtf8 (toS $ takeFileName filePath)
              )
            ]
      responseLBS status200 respHeaders <$> BSL.readFile (rootDir </> filePath)

handleFolderPreview :: Env -> DirectoryID -> IO Response
handleFolderPreview env uuid =
  selectDirectory env uuid >>= maybe404orCont \dir -> do
    subdDirs <- selectSubDirectories env uuid
    files <- selectFilesByDirectory env uuid
    pure $ htmlToResponse $ renderDirectory dir subdDirs files

compressDirectory ::
  ( S.IsStream t,
    MonadIO m,
    MonadBaseControl IO m,
    MonadCatch m,
    Monad (t m)
  ) =>
  Directory ->
  t m Word8
compressDirectory Directory {..} =
  Process.toBytes "tar" $
    toS <$> ["-cvzO", "-C", directoryPath </> "..", directoryName]

handleFolderDownload ::
  (MonadResource m, MonadIO m) =>
  Env ->
  DirectoryID ->
  m Response
handleFolderDownload env uuid = do
  mDir <- liftIO $ selectDirectory env uuid
  case mDir of
    Nothing -> pure $ responseLBS status404 [] mempty
    Just dir@Directory {..} -> do
      let archiveName = directoryName <> ".tgz"
      let fileName = encodeUtf8 (toS $ "syncbox-" <> archiveName)
      let mimeType = defaultMimeLookup $ toS archiveName
      let respHeaders =
            [ ("Content-Type", mimeType),
              ("Content-Disposition", "attachment; filename=" <> fileName)
            ]
      let tgzConduit = Conduit.unfoldM S.uncons $ compressDirectory dir
      pure $
        responseSource status200 respHeaders $
          tgzConduit .| mapC (Chunk . byteString . singleton)

handleIndex :: Env -> IO Response
handleIndex env = do
  root <- selectRootDirectory env
  handleFolderPreview env $ directoryID root

------------------------------------------------------------------------------------------

data ServerOptions = ServerOptions
  { port :: Int,
    root :: FilePath
  }
  deriving (Show, Generic)

instance ParseRecord ServerOptions

--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts' :: ServerOptions <- getRecord "syncbox"
  opts <- do
    absPath <- makeAbsolute $ root opts'
    pure $ opts' & #root .~ absPath
  withConnection ":memory:" $ \db -> do
    let env = Env (root opts) db
    putStrLn ("Initializing" :: Text)
    initDB db
    processRootDirectory env
    putStrLn ("Starting FS Watcher" :: Text)
    withManagerConf defaultConfig {confDebounce = NoDebounce} $ \mgr -> do
      -- start a watching job (in the background)
      void $
        watchTree
          mgr -- manager
          (root opts) -- directory to watch
          (const True) -- predicate
          (processFSEvent env) -- action
      putStrLn ("Starting server" :: Text)
      run (port opts) . logStdout $ makeApplication env
