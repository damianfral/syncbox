{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Database.SQLite.Simple.QQ (sql)
import Protolude hiding (div, head, link, log)
import Servant (err500)
import SyncBox.Types
import System.Directory (getFileSize, makeAbsolute)
import System.FilePath
import Prelude qualified

--------------------------------------------------------------------------------

schema :: [Query]
schema =
  [ [sql|
      CREATE TABLE directories
        ( directory_id TEXT PRIMARY KEY
        , name TEXT NOT NULL
        , path TEXT NOT NULL
        , parent TEXT REFERENCES directories(directory_id) ON DELETE CASCADE
        );
    |],
    [sql|
      CREATE TABLE files
      (
         file_id TEXT NOT NULL,
         name TEXT NOT NULL,
         size INTEGER NOT NULL,
         path TEXT NOT NULL,
         directory TEXT NOT NULL REFERENCES directories(directory_id) ON DELETE CASCADE
      );
    |],
    [sql|CREATE UNIQUE INDEX unique_directory_id ON directories (directory_id); |],
    [sql|CREATE UNIQUE INDEX unique_directory_path ON directories (path);|],
    [sql|CREATE UNIQUE INDEX unique_file_id ON files (file_id);|],
    [sql|CREATE UNIQUE INDEX unique_files_path ON files (path);|]
  ]

-------------------------------------------------------------------------------

initDB :: Connection -> IO ()
initDB = for_ schema . execute_

--------------------------------------------------------------------------------

nextDirectoryID :: (MonadIO m) => m DirectoryID
nextDirectoryID = liftIO $ DirectoryID <$> nextRandom

nextFileID :: IO FileID
nextFileID = FileID <$> nextRandom

--------------------------------------------------------------------------------

selectRootDirectory :: AppM Directory
selectRootDirectory = do
  Env {..} <- ask
  liftIO (headMay <$> query_ dbConnection q) >>= \case
    Just dir -> pure dir
    Nothing -> throwError err500
  where
    q = "select * from directories where parent IS NULL;"

selectDirectory :: DirectoryID -> AppM (Maybe Directory)
selectDirectory uuid = do
  Env {..} <- ask
  liftIO $ headMay <$> query dbConnection q (Only uuid)
  where
    q = "select * from directories where directory_id = ? ORDER BY name ASC;"

selectDirectoryAncestors :: Directory -> AppM [Directory]
selectDirectoryAncestors dir = go (directoryParentID dir) [dir]
  where
    go Nothing acc = pure acc
    go (Just pid) acc =
      selectDirectory pid >>= \case
        Nothing -> pure acc
        Just parent -> go (directoryParentID parent) (parent : acc)

selectDirectoryByFullPath :: FilePath -> AppM (Maybe Directory)
selectDirectoryByFullPath filePath = do
  Env {..} <- ask
  liftIO $ headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from directories where path = ? ORDER BY name ASC;"

selectSubDirectories :: DirectoryID -> AppM [Directory]
selectSubDirectories dirID = do
  Env {..} <- ask
  liftIO $ query dbConnection q $ Only dirID
  where
    q = "select * from directories where parent = ? ORDER BY name ASC;"

selectFile :: FileID -> AppM (Maybe File)
selectFile fileUUID = do
  Env {..} <- ask
  liftIO $ headMay <$> query dbConnection q (Only fileUUID)
  where
    q = "select * from files where file_id = ? ORDER BY name ASC;"

selectFileByFullPath :: FilePath -> AppM (Maybe File)
selectFileByFullPath filePath = do
  Env {..} <- ask
  liftIO $ headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from files where path = ? ORDER BY name ASC;"

selectFilesByDirectory :: DirectoryID -> AppM [File]
selectFilesByDirectory dirID = do
  Env {..} <- ask
  liftIO $ query dbConnection q $ Only dirID
  where
    q = "select * from files where directory = ? ORDER BY name ASC;"

selectDirectorySize :: DirectoryID -> AppM Integer
selectDirectorySize dirID = do
  Env {..} <- ask
  rows <- liftIO $ query dbConnection q $ Only dirID
  pure $ maybe 0 fromOnly $ headMay rows
  where
    q =
      [sql|
        WITH RECURSIVE subtree(id) AS (
          SELECT directory_id FROM directories WHERE directory_id = ?
          UNION ALL
          SELECT directories.directory_id FROM directories
          JOIN subtree ON directories.parent = subtree.id
        )
        SELECT COALESCE(SUM(files.size), 0) FROM files
        WHERE files.directory IN (SELECT id FROM subtree)
      |]

deleteFile :: FileID -> AppM ()
deleteFile fileID = do
  Env {..} <- ask
  let q = "delete from files where file_id = ?;"
  liftIO $ execute dbConnection q $ Only fileID

deleteFileByFullPath :: FilePath -> AppM ()
deleteFileByFullPath fp = do
  Env {..} <- ask
  let q = "delete from files where path = ?;"
  liftIO $ execute dbConnection q $ Only fp

deleteDirectoryByFullPath :: FilePath -> AppM ()
deleteDirectoryByFullPath fp =
  selectDirectoryByFullPath fp >>= \case
    Nothing -> pure ()
    Just dir -> deleteDirectory $ directoryID dir

deleteDirectory :: DirectoryID -> AppM ()
deleteDirectory uuid = do
  Env {..} <- ask
  let q = "delete from directories where directory_id = ?;"
  liftIO $ execute dbConnection q $ Only uuid

insertRootDirectory :: AppM Directory
insertRootDirectory = do
  Env {..} <- ask
  directoryID <- nextDirectoryID
  directoryPath <- liftIO $ makeAbsolute rootDir
  let directoryName = Prelude.last $ splitDirectories directoryPath
  let directoryParentID = Nothing
  let directory = Directory {..}
  let q = "insert into directories values(?,?,?,?);"
  liftIO $ execute dbConnection q directory $> directory

insertDirectory :: FilePath -> AppM Directory
insertDirectory directoryPath = do
  Env {..} <- ask
  let isRootDirectory = makeRelative rootDir directoryPath == "."
  if isRootDirectory
    then selectRootDirectory
    else selectNonRootDirectory dbConnection
  where
    selectNonRootDirectory dbConnection = do
      mDir <- selectDirectoryByFullPath directoryPath
      case mDir of
        Just dir -> pure dir
        Nothing -> do
          -- takeDirectory doesn't work for hidden dirs
          let directoryName = takeFileName directoryPath
              directoryParentPath = takeDirectory directoryPath
          directoryParent <- insertDirectory directoryParentPath
          let directoryParentID = Just $ directoryID directoryParent
          directoryID <- nextDirectoryID
          let dir = Directory {..}
          let q = "insert into directories values(?,?,?,?);"
          liftIO $ execute dbConnection q dir $> dir

insertFile :: FilePath -> AppM ()
insertFile filePath' = do
  Env {..} <- ask
  log I $ "Inserting file: " <> pack filePath'
  filePath <- liftIO $ makeAbsolute filePath'
  let directoryPath = takeDirectory filePath
  dir <- insertDirectory directoryPath
  let fileName = takeFileName filePath
  let fileDirectoryID = directoryID dir
  fileID <- liftIO nextFileID
  fileSize <- liftIO $ getFileSize filePath
  let file = File {..}
  let q = "INSERT or REPLACE INTO files values(?,?,?,?,?);"
  liftIO $ execute dbConnection q file
