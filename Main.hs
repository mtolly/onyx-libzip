{-# LANGUAGE OverloadedStrings #-}
module Main where

import Onyx.Zip
import Foreign
import Foreign.C
import Control.Exception
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Internal (smallChunkSize)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  [path] <- map B8.pack <$> getArgs
  print path
  bracket (zip_open path zip_RDONLY nullPtr) (mapM_ zip_close) $ \mzip -> case mzip of
    Nothing -> putStrLn "error opening zip"
    Just z  -> do
      print z
      n <- zip_get_num_entries z mempty
      zip_set_default_password z $ Just "SGH9ZIP2PASS4MXKR"
      zipAlloca $ \stat -> do
        forM_ [0 .. fromIntegral n - 1] $ \i -> do
          zip_stat_index z i mempty stat >>= \code -> case code of
            0 -> do
              info <- getZipStat stat
              print info
              when (zipStatName info == Just "setlist.info") $ do
                bracket (zip_fopen_index z i mempty) (mapM_ zip_fclose) $ \mfile -> case mfile of
                  Nothing   -> putStrLn "couldn't open setlist.info"
                  Just file -> readZipFile file >>= print
            _ -> putStrLn "zip_stat error"

readZipFile :: ZipFile -> IO BL.ByteString
readZipFile zf = allocaBytes smallChunkSize $ \buf -> let
  loop prev = zip_fread zf buf (fromIntegral smallChunkSize) >>= \n -> case n of
    0         -> return $ BL.fromChunks $ reverse prev
    bytesRead -> do
      chunk <- B.packCStringLen (castPtr buf, fromIntegral bytesRead)
      loop $ chunk : prev
  in loop []
