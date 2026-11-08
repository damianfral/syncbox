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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Database where

import Colog
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (pack)
import Data.Text qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ (sql)
import Protolude hiding (div, head, link, log)
import Servant (err500)
import SyncBox.Core
import System.Directory (getFileSize, makeAbsolute)
import System.FilePath
import Prelude qualified

--------------------------------------------------------------------------------

initialMigration :: Text
initialMigration =
  $(makeRelativeToProject "migrations/20261108-initial.sql" >>= embedStringFile)

migrationStatements :: [Query]
migrationStatements =
  [ Query (T.strip stmt)
  | stmt <- T.splitOn ";" initialMigration,
    not (T.null (T.strip stmt))
  ]

-------------------------------------------------------------------------------

initDB :: Connection -> IO ()
initDB conn = for_ migrationStatements $ execute_ conn

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
  directoryPath <- liftIO $ makeAbsolute rootDir
  let directoryID = directoryIDForPath directoryPath
      directoryName = Prelude.last $ splitDirectories directoryPath
      directoryParentID = Nothing
      directory = Directory {..}
      q = "insert into directories values(?,?,?,?);"
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
          let dir =
                Directory
                  { directoryID = directoryIDForPath directoryPath,
                    directoryName = directoryName,
                    directoryPath = directoryPath,
                    directoryParentID = Just $ directoryID directoryParent
                  }
              q = "insert into directories values(?,?,?,?);"
          liftIO $ execute dbConnection q dir $> dir

insertFile :: FilePath -> AppM ()
insertFile filePath' = do
  Env {..} <- ask
  log I $ "Inserting file: " <> pack filePath'
  filePath <- liftIO $ makeAbsolute filePath'
  let directoryPath = takeDirectory filePath
  dir <- insertDirectory directoryPath
  let fileName = takeFileName filePath
      fileDirectoryID = directoryID dir
      fileID = fileIDForPath filePath
  fileSize <- liftIO $ getFileSize filePath
  let file = File {..}
      q = "INSERT or REPLACE INTO files values(?,?,?,?,?);"
  liftIO $ execute dbConnection q file
