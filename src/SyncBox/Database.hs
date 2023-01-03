{-# LANGUAGE BlockArguments #-}
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

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base64.URL (encodeBase64)
import qualified Data.ByteString.Lazy as BS
import Data.Generics.Labels ()
import Data.UUID (toText)
import Data.UUID.V4
import Database.SQLite.Simple
import qualified NeatInterpolation
import Protolude hiding (div, hash, head, link)
import Protolude.Partial (fromJust)
import SyncBox.Types
import System.Directory (makeAbsolute)
import System.FilePath
import qualified Prelude

--------------------------------------------------------------------------------

schema :: [Query]
schema =
  Query
    <$> [ [NeatInterpolation.text|
      CREATE TABLE directories
        (
           uuid   TEXT PRIMARY KEY,
           name   TEXT NOT NULL,
           path   TEXT NOT NULL,
           parent TEXT REFERENCES directories(id) ON DELETE CASCADE
        );
        |],
          [NeatInterpolation.text|
      CREATE TABLE files
        (
           hash      TEXT NOT NULL,
           name      TEXT NOT NULL,
           path      TEXT NOT NULL,
           directory TEXT NOT NULL REFERENCES directories(id) ON DELETE CASCADE
        );
        |],
          [NeatInterpolation.text|
      CREATE UNIQUE INDEX uniquedirectoriespath ON directories (path);
        |],
          [NeatInterpolation.text|

      CREATE UNIQUE INDEX uniquefileshash ON files (hash);
        |],
          [NeatInterpolation.text|

      CREATE UNIQUE INDEX uniquefilespath ON files (path);
        |]
        ]

-------------------------------------------------------------------------------

initDB :: Connection -> IO ()
initDB conn = mapM_ (execute_ conn) schema

--------------------------------------------------------------------------------

nextDirectoryID :: IO DirectoryID
nextDirectoryID = DirectoryID . toText <$> nextRandom

hashFile :: FilePath -> IO Hash
hashFile filepath = do
  fileContent <- BS.toStrict <$> BS.readFile filepath
  pure $ Hash $ encodeBase64 $ hash fileContent

--------------------------------------------------------------------------------

selectRootDirectory :: Env -> IO Directory
selectRootDirectory Env {..} = Prelude.head <$> query_ dbConnection q
  where
    q = "select * from directories where parent IS NULL;"

selectDirectory :: Env -> DirectoryID -> IO (Maybe Directory)
selectDirectory Env {..} uuid = Protolude.headMay <$> query dbConnection q (Only uuid)
  where
    q = "select * from directories where uuid = ?;"

selectDirectoryByFullPath :: Env -> FilePath -> IO (Maybe Directory)
selectDirectoryByFullPath Env {..} filePath =
  Protolude.headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from directories where path = ?;"

selectSubDirectories :: Env -> DirectoryID -> IO [Directory]
selectSubDirectories Env {..} = query dbConnection q . Only
  where
    q = "select * from directories where parent = ?;"

selectFile :: Env -> Hash -> IO (Maybe File)
selectFile Env {..} fileHash =
  Protolude.headMay <$> query dbConnection q (Only fileHash)
  where
    q = "select * from files where hash = ?;"

selectFileByFullPath :: Env -> FilePath -> IO (Maybe File)
selectFileByFullPath Env {..} filePath =
  Protolude.headMay <$> query dbConnection q (Only filePath)
  where
    q = "select * from files where path = ?;"

selectFilesByDirectory :: Env -> DirectoryID -> IO [File]
selectFilesByDirectory Env {..} = query dbConnection q . Only
  where
    q = "select * from files where directory = ?;"

updateFileByFullPath :: Env -> FilePath -> IO ()
updateFileByFullPath Env {..} =
  execute dbConnection "update files set hash = ?;" . Only <=< hashFile

deleteFile :: Env -> Hash -> IO ()
deleteFile Env {..} =
  execute dbConnection "delete from files where hash = ?;" . Only

deleteFileByFullPath :: Env -> FilePath -> IO ()
deleteFileByFullPath Env {..} =
  execute dbConnection "delete from files where path = ?;" . Only

deleteDirectoryByFullPath :: Env -> FilePath -> IO ()
deleteDirectoryByFullPath env =
  selectDirectoryByFullPath env >=> \case
    Nothing -> pure ()
    Just dir -> deleteDirectory env . directoryID $ dir

deleteDirectory :: Env -> DirectoryID -> IO ()
deleteDirectory Env {..} uuid =
  execute dbConnection "delete from directories where uuid = ?;" $ Only uuid

insertRootDirectory :: Env -> IO Directory
insertRootDirectory env@Env {..} = do
  directoryID <- nextDirectoryID
  directoryPath <- makeAbsolute rootDir
  let directoryName = Prelude.last $ splitDirectories directoryPath
  let directoryParentID = Nothing
  let directory = Directory {..}
  execute dbConnection "insert into directories values(?,?,?,?);" directory
  selectRootDirectory env

insertDirectory :: Env -> FilePath -> IO Directory
insertDirectory env@Env {..} directoryPath
  | isRootDirectory = selectRootDirectory env
  | otherwise = do
    mDir <- selectDirectoryByFullPath env directoryPath
    case mDir of
      Just dir -> pure dir
      Nothing -> do
        let directoryName = takeBaseName directoryPath
            directoryParentPath = takeDirectory directoryPath
        directoryParent <- insertDirectory env directoryParentPath
        let directoryParentID = Just $ directoryID directoryParent
        directoryID <- nextDirectoryID
        let dir = Directory {..}
        execute dbConnection "insert into directories values(?,?,?,?);" dir
        fromJust <$> selectDirectory env directoryID
  where
    isRootDirectory = makeRelative rootDir directoryPath == "."

insertFile :: Env -> FilePath -> IO ()
insertFile env@Env {..} filePath' = withTransaction dbConnection $ do
  filePath <- makeAbsolute filePath'
  let directoryPath = takeDirectory filePath
  dir <- insertDirectory env directoryPath
  let fileName = takeFileName filePath
  let fileDirectoryID = directoryID dir
  fileHash <- hashFile filePath
  let file = File {..}
  execute dbConnection "INSERT or REPLACE INTO files values(?,?,?,?);" file
