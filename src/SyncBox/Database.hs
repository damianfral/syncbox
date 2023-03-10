{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Database where

import Colog
import Data.Text (pack)
import Data.UUID.V4
import Database.SQLite.Simple
import qualified NeatInterpolation
import Protolude hiding (div, head, link, log)
import Protolude.Partial (fromJust)
import SyncBox.Types
import System.Directory (getFileSize, makeAbsolute)
import System.FilePath
import qualified Prelude

--------------------------------------------------------------------------------

schema :: [Query]
schema =
  Query
    <$> [ [NeatInterpolation.text|
      CREATE TABLE directories
        (
           directory_id TEXT PRIMARY KEY,
           name         TEXT NOT NULL,
           path         TEXT NOT NULL,
           parent       TEXT REFERENCES directories(directory_id) ON DELETE CASCADE
        );
        |],
          [NeatInterpolation.text|
      CREATE TABLE files
        (
           file_id   TEXT NOT NULL,
           name      TEXT NOT NULL,
           size      INTEGER NOT NULL,
           path      TEXT NOT NULL,
           directory TEXT NOT NULL REFERENCES directories(directory_id) ON DELETE CASCADE
        );
        |],
          [NeatInterpolation.text|

      CREATE UNIQUE INDEX unique_directory_id ON directories (directory_id);
        |],
          [NeatInterpolation.text|
      CREATE UNIQUE INDEX unique_directory_path ON directories (path);
        |],
          [NeatInterpolation.text|

      CREATE UNIQUE INDEX unique_file_id ON files (file_id);
        |],
          [NeatInterpolation.text|

      CREATE UNIQUE INDEX unique_files_path ON files (path);
        |]
        ]

-------------------------------------------------------------------------------

initDB :: Connection -> IO ()
initDB conn = mapM_ (execute_ conn) schema

--------------------------------------------------------------------------------

nextDirectoryID :: MonadIO m => m DirectoryID
nextDirectoryID = liftIO $ DirectoryID <$> nextRandom

nextFileID :: IO FileID
nextFileID = FileID <$> nextRandom

--------------------------------------------------------------------------------

selectRootDirectory :: AppM Directory
selectRootDirectory = do
  Env {..} <- ask
  liftIO $ Prelude.head <$> query_ dbConnection q
  where
    q = "select * from directories where parent IS NULL;"

selectDirectory :: DirectoryID -> AppM (Maybe Directory)
selectDirectory uuid = do
  Env {..} <- ask
  liftIO $ Protolude.headMay <$> query dbConnection q (Only uuid)
  where
    q = "select * from directories where directory_id = ? ORDER BY name ASC;"

selectDirectoryByFullPath :: FilePath -> AppM (Maybe Directory)
selectDirectoryByFullPath filePath = do
  Env {..} <- ask
  liftIO $ Protolude.headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from directories where path = ?; ORDER BY name ASC"

selectSubDirectories :: DirectoryID -> AppM [Directory]
selectSubDirectories dirID = do
  Env {..} <- ask
  liftIO $ query dbConnection q $ Only dirID
  where
    q = "select * from directories where parent = ? ORDER BY name ASC;"

selectFile :: FileID -> AppM (Maybe File)
selectFile fileUUID = do
  Env {..} <- ask
  liftIO $ Protolude.headMay <$> query dbConnection q (Only fileUUID)
  where
    q = "select * from files where file_id = ? ORDER BY name ASC;"

selectFileByFullPath :: FilePath -> AppM (Maybe File)
selectFileByFullPath filePath = do
  Env {..} <- ask
  liftIO $ Protolude.headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from files where path = ? ORDER BY name ASC;"

selectFilesByDirectory :: DirectoryID -> AppM [File]
selectFilesByDirectory dirID = do
  Env {..} <- ask
  liftIO $ query dbConnection q $ Only dirID
  where
    q = "select * from files where directory = ? ORDER BY name ASC;"

deleteFile :: FileID -> AppM ()
deleteFile fileID = do
  Env {..} <- ask
  liftIO $ execute dbConnection "delete from files where file_id = ?;" $ Only fileID

deleteFileByFullPath :: FilePath -> AppM ()
deleteFileByFullPath fp = do
  Env {..} <- ask
  liftIO $ execute dbConnection "delete from files where path = ?;" $ Only fp

deleteDirectoryByFullPath :: FilePath -> AppM ()
deleteDirectoryByFullPath fp = do
  selectDirectoryByFullPath fp >>= \case
    Nothing -> pure ()
    Just dir -> deleteDirectory $ directoryID dir

deleteDirectory :: DirectoryID -> AppM ()
deleteDirectory uuid = do
  Env {..} <- ask
  liftIO $ execute dbConnection "delete from directories where directory_id = ?;" $ Only uuid

insertRootDirectory :: AppM Directory
insertRootDirectory = do
  Env {..} <- ask
  directoryID <- nextDirectoryID
  directoryPath <- liftIO $ makeAbsolute rootDir
  let directoryName = Prelude.last $ splitDirectories directoryPath
  let directoryParentID = Nothing
  let directory = Directory {..}
  liftIO $ execute dbConnection "insert into directories values(?,?,?,?);" directory
  selectRootDirectory

insertDirectory :: FilePath -> AppM Directory
insertDirectory directoryPath = do
  Env {..} <- ask
  let isRootDirectory = makeRelative rootDir directoryPath == "."
  if isRootDirectory
    then selectRootDirectory
    else do
      mDir <- selectDirectoryByFullPath directoryPath
      case mDir of
        Just dir -> pure dir
        Nothing -> do
          let directoryName = takeBaseName directoryPath
              directoryParentPath = takeDirectory directoryPath
          directoryParent <- insertDirectory directoryParentPath
          let directoryParentID = Just $ directoryID directoryParent
          directoryID <- nextDirectoryID
          let dir = Directory {..}
          liftIO $ execute dbConnection "insert into directories values(?,?,?,?);" dir
          fromJust <$> selectDirectory directoryID

insertFile :: FilePath -> AppM ()
insertFile filePath' = do
  env@(Env {..}) <- ask
  log I $ "Inserting file: " <> pack filePath'
  liftIO $ withTransaction dbConnection $ do
    filePath <- makeAbsolute filePath'
    let directoryPath = takeDirectory filePath
    Right dir <- runAppM env $ insertDirectory directoryPath
    let fileName = takeFileName filePath
    let fileDirectoryID = directoryID dir
    fileID <- nextFileID
    fileSize <- getFileSize filePath
    let file = File {..}
    execute dbConnection "INSERT or REPLACE INTO files values(?,?,?,?,?);" file
