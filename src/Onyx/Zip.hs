{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Zip where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Foreign
import Foreign.C
import qualified Data.Map as Map
import qualified Data.ByteString as B

newtype Zip = Zip (Ptr Zip)

newtype ZipFlags = ZipFlags Word32

C.context $ C.baseCtx <> C.bsCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "zip_t", [t|Zip|])
    , (C.TypeName "zip_flags_t", [t|ZipFlags|])
    ]
  }

C.include "<zip.h>"

-- ZIP_EXTERN int zip_close(zip_t *_Nonnull);
zipClose :: Zip -> IO CInt
zipClose (Zip z) = [C.exp| int { zip_close($(zip_t *z)) } |]

-- ZIP_EXTERN int zip_delete(zip_t *_Nonnull, zip_uint64_t);
zipDelete :: Zip -> Word64 -> IO CInt
zipDelete (Zip z) n = [C.exp| int { zip_delete($(zip_t *z), $(uint64_t n)) } |]

-- ZIP_EXTERN zip_int64_t zip_dir_add(zip_t *_Nonnull, const char *_Nonnull, zip_flags_t);
zipDirAdd :: Zip -> B.ByteString -> ZipFlags -> IO Int64
zipDirAdd (Zip z) str flags = [C.exp| int64_t { zip_dir_add($(zip_t *z), $bs-ptr:str, $(zip_flags_t flags)) } |]
