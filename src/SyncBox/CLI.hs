{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.CLI where

import Colog (log)
import Colog.Actions
import Colog.Core
import Conduit
import Database.SQLite.Simple
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Generic
import Protolude hiding (Handler, div, head, link, log)
import Servant.Conduit ()
import SyncBox.Core
import SyncBox.Database
import SyncBox.Server
import System.Directory (makeAbsolute)
import System.FSNotify as FS

data ServerOptions w = ServerOptions
  { port :: w ::: Int <!> "3333" <?> "port",
    root :: w ::: FilePath <!> "." <?> "directory to server",
    hidden :: w ::: Bool <?> "include hidden directories and files"
  }
  deriving (Generic)

instance ParseRecord (ServerOptions Wrapped)

runCLI :: IO ()
runCLI = do
  opts' <- unwrapRecord "syncbox"
  opts <- do
    absPath <- makeAbsolute $ root opts'
    pure $ opts' {root = absPath}
  withConnection ":memory:" $ \db -> do
    let env = Env (root opts) (hidden opts) db richMessageAction
    initDB db
    void $ runAppM env processRootDirectory
    withManager $ \mgr -> void $ runAppM env $ watcher opts mgr env
  where
    watcher :: ServerOptions Unwrapped -> WatchManager -> Env AppM -> AppM ()
    watcher opts mgr env = do
      let dir = root opts
      let actionPredicate = const True
      let runApp = void . runAppM env . processFSEvent
      log I "Starting FS Watcher"
      void $ liftIO $ watchTree mgr dir actionPredicate runApp
      log I "Starting server"
      liftIO $ run (port opts) . logStdout $ app env
