module SyncBox.ThumbnailSpec (spec) where

import SyncBox.Thumbnail
import Test.Syd

spec :: Spec
spec = describe "thumbnailType" $ do
  it "detects image formats" $ do
    thumbnailType "photo.jpg" `shouldBe` Just ImageThumbnail
    thumbnailType "photo.JPEG" `shouldBe` Just ImageThumbnail
    thumbnailType "image.png" `shouldBe` Just ImageThumbnail
    thumbnailType "icon.gif" `shouldBe` Just ImageThumbnail
    thumbnailType "picture.webp" `shouldBe` Just ImageThumbnail
    thumbnailType "vector.svg" `shouldBe` Just ImageThumbnail

  it "detects video formats" $ do
    thumbnailType "clip.mp4" `shouldBe` Just VideoThumbnail
    thumbnailType "clip.MKV" `shouldBe` Just VideoThumbnail
    thumbnailType "clip.mov" `shouldBe` Just VideoThumbnail
    thumbnailType "clip.webm" `shouldBe` Just VideoThumbnail
    thumbnailType "clip.avi" `shouldBe` Just VideoThumbnail

  it "ignores unsupported files" $ do
    thumbnailType "notes.txt" `shouldBe` Nothing
    thumbnailType "archive.tar.gz" `shouldBe` Nothing
    thumbnailType "README" `shouldBe` Nothing
