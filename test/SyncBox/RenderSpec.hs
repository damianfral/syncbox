{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.RenderSpec (spec) where

import Data.UUID qualified as UUID
import Protolude
import SyncBox.Core
import SyncBox.Render
import Test.Syd

spec :: Spec
spec = do
  describe "renderFileSize" $ do
    it "formats bytes" $ renderFileSize 512 `shouldBe` "512 B"

    it "formats kilobytes" $ renderFileSize 2048 `shouldBe` "2 KB"

    it "formats megabytes" $
      renderFileSize (5 * 1024 * 1024) `shouldBe` "5.0 MB"

    it "formats gigabytes" $
      renderFileSize (3 * 1024 * 1024 * 1024) `shouldBe` "3.0 GB"

  describe "fileKind" $ do
    it "returns uppercased extension" $
      fileKind (mkFile "photo.jpg") `shouldBe` "JPG"

    it "returns File when there is no extension" $
      fileKind (mkFile "README") `shouldBe` "File"

mkFile :: FilePath -> File
mkFile name =
  File
    { fileID = FileID UUID.nil,
      fileName = name,
      fileSize = 0,
      filePath = "/tmp/" <> name,
      fileDirectoryID = DirectoryID UUID.nil
    }
