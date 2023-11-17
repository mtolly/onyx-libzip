{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Language.C.Inline as C
import qualified Data.ByteString.Char8 as B8
import System.Environment (getArgs)

C.context (C.baseCtx <> C.bsCtx)

C.include "<zip.h>"
C.include "<stdio.h>"

main :: IO ()
main = do
  [path] <- map B8.pack <$> getArgs
  [C.block| void {
    int error = 0;
    zip_t *za = zip_open($bs-ptr:path, 0, &error);
    printf("%p\n", za);
  } |]
