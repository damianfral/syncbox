{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SyncBox.Thumbnail where

import Data.ByteString.Lazy qualified as LBS
import Data.Text (pack)
import Data.Text qualified as T
import Protolude
import SyncBox.Core
import System.Directory
import System.FilePath (takeExtension, (</>))
import System.Process.Typed
import Prelude (String)

data ThumbnailType = ImageThumbnail | VideoThumbnail
  deriving (Eq, Show)

thumbnailType :: FilePath -> Maybe ThumbnailType
thumbnailType path = case drop 1 $ toLower <$> takeExtension path of
  "jpeg" -> Just ImageThumbnail
  "jpg" -> Just ImageThumbnail
  "png" -> Just ImageThumbnail
  "gif" -> Just ImageThumbnail
  "webp" -> Just ImageThumbnail
  "mov" -> Just VideoThumbnail
  "mkv" -> Just VideoThumbnail
  "mp4" -> Just VideoThumbnail
  "webm" -> Just VideoThumbnail
  "avi" -> Just VideoThumbnail
  _ -> Nothing

ensureThumbnail :: File -> IO (Either Text FilePath)
ensureThumbnail file = case thumbnailType (filePath file) of
  Nothing -> pure $ Left "file type has no thumbnail"
  Just kind -> do
    cache <- getXdgDirectory XdgCache "syncbox/thumbnails"
    createDirectoryIfMissing True cache
    let output = cache </> show (unFileID $ fileID file) <> ".jpg"
    exists <- doesFileExist output
    if exists
      then pure $ Right output
      else generate kind (filePath file) output

generate :: ThumbnailType -> FilePath -> FilePath -> IO (Either Text FilePath)
generate ImageThumbnail input output = do
  let args =
        [ input <> "[0]",
          "-auto-orient",
          "-thumbnail",
          "256x>",
          "-quality",
          "82",
          output
        ]
  result <- runCommand "convert" args >>= verifyOutput output
  case result of
    Right () -> pure $ Right output
    Left err -> pure $ Left $ "convert failed: " <> err
generate VideoThumbnail input output = do
  let ffmpegFrame i = ffmpegArgs i input output
  generated <- runCommand "ffmpeg" (ffmpegFrame 1) >>= verifyOutput output
  case generated of
    Right () -> pure $ Right output
    Left err1 -> do
      removeOutput output
      generated0 <- runCommand "ffmpeg" (ffmpegFrame 0) >>= verifyOutput output
      pure $ case generated0 of
        Right () -> Right output
        Left err0 ->
          Left $ "ffmpeg seek=1 failed: " <> err1 <> "; seek=0 failed: " <> err0

verifyOutput :: FilePath -> Either Text () -> IO (Either Text ())
verifyOutput output result = case result of
  Left err -> pure $ Left err
  Right () -> do
    exists <- doesFileExist output
    if exists
      then pure $ Right ()
      else pure $ Left "command succeeded but thumbnail file was not created"

ffmpegArgs :: Int -> FilePath -> FilePath -> [String]
ffmpegArgs position input output =
  [ "-y",
    "-loglevel",
    "error",
    "-ss",
    show position,
    "-i",
    input,
    "-frames:v",
    "1",
    "-vf",
    "scale=512:-2:force_original_aspect_ratio=decrease",
    "-q:v",
    "3",
    output
  ]

runCommand :: FilePath -> [String] -> IO (Either Text ())
runCommand command arguments = do
  result <- try @IOException $ do
    readProcess $
      setStdout byteStringOutput $
        setStderr byteStringOutput $
          proc command arguments

  pure $ case result of
    Right (ExitSuccess, _, _) -> Right ()
    Right (code, out, err) ->
      Left $ show code <> summarizeOutput out <> summarizeOutput err
    Left ex ->
      Left $ pack command <> " could not run: " <> pack (displayException ex)

summarizeOutput :: LBS.ByteString -> Text
summarizeOutput value =
  let trimmed = T.strip $ decodeUtf8With lenientDecode $ LBS.toStrict value
   in if T.null trimmed then "" else " " <> T.take 300 trimmed

removeOutput :: FilePath -> IO ()
removeOutput path = do
  exists <- doesFileExist path
  when exists $ void $ try @IOException $ removeFile path
