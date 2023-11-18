{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Onyx.Zip where

import Foreign
import Foreign.C
import qualified Data.ByteString as B
import Data.ByteString (useAsCString, packCString)
import Data.Coerce (Coercible, coerce)
import Control.Monad (guard)
import Control.Exception (bracket)

#include "zip.h"

{#pointer *zip_t as Zip newtype #}
deriving instance Storable Zip
deriving instance Show Zip

{#pointer *zip_error_t as ZipError newtype #}
deriving instance Storable ZipError
deriving instance Show ZipError

{#pointer *zip_source_t as ZipSource newtype #}
deriving instance Storable ZipSource
deriving instance Show ZipSource

{#pointer *zip_file_t as ZipFile newtype #}
deriving instance Storable ZipFile
deriving instance Show ZipFile

{#pointer *zip_file_attributes_t as ZipFileAttributes newtype #}
deriving instance Storable ZipFileAttributes
deriving instance Show ZipFileAttributes

{#pointer *zip_stat_t as ZipStat newtype #}
deriving instance Storable ZipStat
deriving instance Show ZipStat

{#pointer *zip_buffer_fragment_t as ZipBufferFragment newtype #}
deriving instance Storable ZipBufferFragment
deriving instance Show ZipBufferFragment

{#enum zip_source_cmd as ZipSourceCmd {} #}
deriving instance Show ZipSourceCmd

newtype ZipFlags = ZipFlags Word32
  deriving (Storable, Show)
{#typedef zip_flags_t ZipFlags #}

instance Semigroup ZipFlags where
  ZipFlags x <> ZipFlags y = ZipFlags (x .|. y)
instance Monoid ZipFlags where
  mempty = ZipFlags 0

newtype ZipOpenFlags = ZipOpenFlags CInt
  deriving (Storable, Show)

instance Semigroup ZipOpenFlags where
  ZipOpenFlags x <> ZipOpenFlags y = ZipOpenFlags (x .|. y)
instance Monoid ZipOpenFlags where
  mempty = ZipOpenFlags 0

zip_CREATE, zip_EXCL, zip_CHECKCONS, zip_TRUNCATE, zip_RDONLY :: ZipOpenFlags
zip_CREATE    = ZipOpenFlags {#const ZIP_CREATE    #}
zip_EXCL      = ZipOpenFlags {#const ZIP_EXCL      #}
zip_CHECKCONS = ZipOpenFlags {#const ZIP_CHECKCONS #}
zip_TRUNCATE  = ZipOpenFlags {#const ZIP_TRUNCATE  #}
zip_RDONLY    = ZipOpenFlags {#const ZIP_RDONLY    #}

{#typedef zip_int8_t   Int8   #}
{#typedef zip_uint8_t  Word8  #}
{#typedef zip_int16_t  Int16  #}
{#typedef zip_uint16_t Word16 #}
{#typedef zip_int32_t  Int32  #}
{#typedef zip_uint32_t Word32 #}
{#typedef zip_int64_t  Int64  #}
{#typedef zip_uint64_t Word64 #}

nullableIn :: (Coercible (Ptr ()) a) => Maybe a -> a
nullableIn Nothing  = coerce (nullPtr :: Ptr ())
nullableIn (Just x) = x

nullableOut :: (Coercible a (Ptr ())) => a -> Maybe a
nullableOut x = guard ((coerce x :: Ptr ()) /= nullPtr) >> Just x

nullableInString :: Maybe B.ByteString -> (CString -> IO a) -> IO a
nullableInString Nothing   f = f nullPtr
nullableInString (Just bs) f = useAsCString bs f

nullableOutString :: CString -> IO (Maybe B.ByteString)
nullableOutString p = if p == nullPtr
  then return Nothing
  else Just <$> B.packCString p

class ZipAllocate a where
  zipNew :: IO a
  zipFree :: a -> IO ()

zipAlloca :: (ZipAllocate a) => (a -> IO b) -> IO b
zipAlloca = bracket zipNew zipFree

instance ZipAllocate ZipStat where
  zipNew = do
    x <- ZipStat <$> mallocBytes {#sizeof zip_stat_t #}
    zip_stat_init x
    return x
  zipFree (ZipStat p) = free p

data ZipStatHs = ZipStatHs
  { zipStatValid            :: Word64
  , zipStatName             :: Maybe B.ByteString
  , zipStatIndex            :: Word64
  , zipStatSize             :: Word64
  , zipStatCompSize         :: Word64
  , zipStatMTime            :: CTime
  , zipStatCRC              :: Word32
  , zipStatCompMethod       :: Word16
  , zipStatEncryptionMethod :: Word16
  , zipStatFlags            :: Word32
  } deriving (Show)

getZipStat :: ZipStat -> IO ZipStatHs
getZipStat zs = do
  a <- coerce <$> {#get zip_stat_t->valid             #} zs
  b <-            {#get zip_stat_t->name              #} zs >>= nullableOutString
  c <- coerce <$> {#get zip_stat_t->index             #} zs
  d <- coerce <$> {#get zip_stat_t->size              #} zs
  e <- coerce <$> {#get zip_stat_t->comp_size         #} zs
  f <- coerce <$> {#get zip_stat_t->mtime             #} zs
  g <- coerce <$> {#get zip_stat_t->crc               #} zs
  h <- coerce <$> {#get zip_stat_t->comp_method       #} zs
  i <- coerce <$> {#get zip_stat_t->encryption_method #} zs
  j <- coerce <$> {#get zip_stat_t->flags             #} zs
  return ZipStatHs
    { zipStatValid            = a
    , zipStatName             = b
    , zipStatIndex            = c
    , zipStatSize             = d
    , zipStatCompSize         = e
    , zipStatMTime            = f
    , zipStatCRC              = g
    , zipStatCompMethod       = h
    , zipStatEncryptionMethod = i
    , zipStatFlags            = j
    }

instance ZipAllocate ZipError where
  zipNew = do
    x <- ZipError <$> mallocBytes {#sizeof zip_error_t #}
    zip_error_init x
    return x
  zipFree x@(ZipError p) = do
    zip_error_fini x
    free p

data ZipErrorHs = ZipErrorHs
  { zipErrorCodeZip    :: CInt
  , zipErrorCodeSystem :: CInt
  , zipErrorString     :: B.ByteString
  } deriving (Show)

getZipError :: ZipError -> IO ZipErrorHs
getZipError err = do
  zip_err <- zip_error_code_zip    err
  sys_err <- zip_error_code_system err
  str     <- zip_error_strerror    err
  return ZipErrorHs
    { zipErrorCodeZip    = zip_err
    , zipErrorCodeSystem = sys_err
    , zipErrorString     = str
    }

zipEither :: (Maybe ZipError -> IO (Maybe a)) -> IO (Either ZipErrorHs a)
zipEither f = zipAlloca $ \err -> f (Just err) >>= \mx -> case mx of
  Nothing -> Left <$> getZipError err
  Just x  -> return $ Right x

-----------------------------------------------------------

-- variadic functions, callbacks, and windows-only functions omitted for now

{#fun zip_close                         { `Zip' } -> `()' #}
{#fun zip_delete                        { `Zip', `Word64' } -> `CInt' #}
{#fun zip_dir_add                       { `Zip', useAsCString* `B.ByteString', `ZipFlags' } -> `Int64' #}
{#fun zip_discard                       { `Zip' } -> `()' #}

{#fun zip_get_error as zip_get_error    { `Zip' } -> `ZipError' #}
{#fun zip_error_clear                   { `Zip' } -> `()' #}
{#fun zip_error_code_zip                { `ZipError' } -> `CInt' #}
{#fun zip_error_code_system             { `ZipError' } -> `CInt' #}
{#fun zip_error_fini                    { `ZipError' } -> `()' #}
{#fun zip_error_init                    { `ZipError' } -> `()' #}
{#fun zip_error_init_with_code          { `ZipError', `CInt' } -> `()' #}
{#fun zip_error_set                     { nullableIn `Maybe ZipError', `CInt', `CInt' } -> `()' #}
{#fun zip_error_set_from_source         { `ZipError', nullableIn `Maybe ZipSource' } -> `()' #}
{#fun zip_error_strerror                { `ZipError' } -> `B.ByteString' packCString* #}
{#fun zip_error_system_type             { `ZipError' } -> `CInt' #}
{#fun zip_error_to_data                 { `ZipError', `Ptr ()', `Word64' } -> `Int64' #}

{#fun zip_fclose                        { `ZipFile' } -> `CInt' #}
{#fun zip_fdopen                        { `CInt', `CInt', id `Ptr CInt' } -> `Maybe Zip' nullableOut #}
{#fun zip_file_add                      { `Zip', useAsCString* `B.ByteString', `ZipSource', `ZipFlags' } -> `Int64' #}
{#fun zip_file_attributes_init          { `ZipFileAttributes' } -> `()' #}
{#fun zip_file_error_clear              { `ZipFile' } -> `()' #}
{#fun zip_file_extra_field_delete       { `Zip', `Word64', `Word16', `ZipFlags' } -> `CInt' #}
{#fun zip_file_extra_field_delete_by_id { `Zip', `Word64', `Word16', `Word16', `ZipFlags' } -> `CInt' #}
{#fun zip_file_extra_field_set          { `Zip', `Word64', `Word16', `Word16', id `Ptr Word8', `Word16', `ZipFlags' } -> `CInt' #}
{#fun zip_file_extra_fields_count       { `Zip', `Word64', `ZipFlags' } -> `Int16' #}
{#fun zip_file_extra_fields_count_by_id { `Zip', `Word64', `Word16', `ZipFlags' } -> `Int16' #}
{#fun zip_file_extra_field_get          { `Zip', `Word64', `Word16', id `Ptr Word16', id `Ptr Word16', `ZipFlags' } -> `Ptr Word8' id #}
{#fun zip_file_extra_field_get_by_id    { `Zip', `Word64', `Word16', `Word16', id `Ptr Word16', `ZipFlags' } -> `Ptr Word8' id #}
{#fun zip_file_get_comment              { `Zip', `Word64', id `Ptr Word32', `ZipFlags' } -> `Maybe B.ByteString' nullableOutString* #}
{#fun zip_file_get_error                { `ZipFile' } -> `ZipError' #}
{#fun zip_file_get_external_attributes  { `Zip', `Word64', `ZipFlags', id `Ptr Word8', id `Ptr Word32' } -> `CInt' #}
{#fun zip_file_is_seekable              { `ZipFile' } -> `CInt' #}
{#fun zip_file_rename                   { `Zip', `Word64', useAsCString* `B.ByteString', `ZipFlags' } -> `CInt' #}
{#fun zip_file_replace                  { `Zip', `Word64', `ZipSource', `ZipFlags' } -> `CInt' #}
{#fun zip_file_set_comment              { `Zip', `Word64', nullableInString* `Maybe B.ByteString', `Word16', `ZipFlags' } -> `CInt' #}
{#fun zip_file_set_dostime              { `Zip', `Word64', `Word16', `Word16', `ZipFlags' } -> `CInt' #}
{#fun zip_file_set_encryption           { `Zip', `Word64', `Word16', nullableInString* `Maybe B.ByteString' } -> `CInt' #}
{#fun zip_file_set_external_attributes  { `Zip', `Word64', `ZipFlags', `Word8', `Word32' } -> `CInt' #}
{#fun zip_file_set_mtime                { `Zip', `Word64', coerce `CTime', `ZipFlags' } -> `CInt' #}
{#fun zip_file_strerror                 { `ZipFile' } -> `B.ByteString' packCString* #}
{#fun zip_fopen                         { `Zip', useAsCString* `B.ByteString', `ZipFlags' } -> `Maybe ZipFile' nullableOut #}
{#fun zip_fopen_encrypted               { `Zip', useAsCString* `B.ByteString', `ZipFlags', nullableInString* `Maybe B.ByteString' } -> `Maybe ZipFile' nullableOut #}
{#fun zip_fopen_index                   { `Zip', `Word64', `ZipFlags' } -> `Maybe ZipFile' nullableOut #}
{#fun zip_fopen_index_encrypted         { `Zip', `Word64', `ZipFlags', nullableInString* `Maybe B.ByteString' } -> `Maybe ZipFile' nullableOut #}
{#fun zip_fread                         { `ZipFile', id `Ptr ()', `Word64' } -> `Int64' #}
{#fun zip_fseek                         { `ZipFile', `Int64', `CInt' } -> `Int8' #}
{#fun zip_ftell                         { `ZipFile' } -> `Int64' #}
{#fun zip_get_archive_comment           { `Zip', id `Ptr CInt', `ZipFlags' } -> `Maybe B.ByteString' nullableOutString* #}
{#fun zip_get_archive_flag              { `Zip', `ZipFlags', `ZipFlags' } -> `CInt' #}
{#fun zip_get_name                      { `Zip', `Word64', `ZipFlags' } -> `Maybe B.ByteString' nullableOutString* #}
{#fun zip_get_num_entries               { `Zip', `ZipFlags' } -> `Int64' #}
{#fun zip_libzip_version                { } -> `B.ByteString' packCString* #}
{#fun zip_name_locate                   { `Zip', useAsCString* `B.ByteString', `ZipFlags' } -> `Int64' #}
{#fun zip_open                          { useAsCString* `B.ByteString', coerce `ZipOpenFlags', id `Ptr CInt' } -> `Maybe Zip' nullableOut #}
{#fun zip_open_from_source              { `ZipSource', coerce `ZipOpenFlags', nullableIn `Maybe ZipError' } -> `Maybe Zip' nullableOut #}
-- ZIP_EXTERN int zip_register_progress_callback_with_state(zip_t *_Nonnull, double, zip_progress_callback _Nullable, void (*_Nullable)(void *_Nullable), void *_Nullable);
-- ZIP_EXTERN int zip_register_cancel_callback_with_state(zip_t *_Nonnull, zip_cancel_callback _Nullable, void (*_Nullable)(void *_Nullable), void *_Nullable);
{#fun zip_set_archive_comment           { `Zip', nullableInString* `Maybe B.ByteString', `Word16' } -> `CInt' #}
{#fun zip_set_archive_flag              { `Zip', `ZipFlags', `CInt' } -> `CInt' #}
{#fun zip_set_default_password          { `Zip', nullableInString* `Maybe B.ByteString' } -> `CInt' #}
{#fun zip_set_file_compression          { `Zip', `Word64', `Int32', `Word32' } -> `CInt' #}
{#fun zip_source_begin_write            { `ZipSource' } -> `CInt' #}
{#fun zip_source_begin_write_cloning    { `ZipSource', `Word64' } -> `CInt' #}
{#fun zip_source_buffer                 { `Zip', `Ptr ()', `Word64', `CInt' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_buffer_create          { `Ptr ()', `Word64', `CInt', nullableIn `Maybe ZipError' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_buffer_fragment        { `Zip', `ZipBufferFragment', `Word64', `CInt' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_buffer_fragment_create { nullableIn `Maybe ZipBufferFragment', `Word64', `CInt', nullableIn `Maybe ZipError' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_close                  { `ZipSource' } -> `CInt' #}
{#fun zip_source_commit_write           { `ZipSource' } -> `CInt' #}
{#fun zip_source_error                  { `ZipSource' } -> `ZipError' #}
{#fun zip_source_file                   { `Zip', useAsCString* `B.ByteString', `Word64', `Int64' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_file_create            { useAsCString* `B.ByteString', `Word64', `Int64', nullableIn `Maybe ZipError' } -> `Maybe ZipSource' nullableOut #}
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_filep(zip_t *_Nonnull, FILE *_Nonnull, zip_uint64_t, zip_int64_t);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_filep_create(FILE *_Nonnull, zip_uint64_t, zip_int64_t, zip_error_t *_Nullable);
{#fun zip_source_free                   { nullableIn `Maybe ZipSource' } -> `()' #}
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_function(zip_t *_Nonnull, zip_source_callback _Nonnull, void *_Nullable);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_function_create(zip_source_callback _Nonnull, void *_Nullable, zip_error_t *_Nullable);
{#fun zip_source_get_file_attributes    { `ZipSource', `ZipFileAttributes' } -> `CInt' #}
{#fun zip_source_is_deleted             { `ZipSource' } -> `CInt' #}
{#fun zip_source_is_seekable            { `ZipSource' } -> `CInt' #}
{#fun zip_source_keep                   { `ZipSource' } -> `()' #}
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_layered(zip_t *_Nullable, zip_source_t *_Nonnull, zip_source_layered_callback _Nonnull, void *_Nullable);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_layered_create(zip_source_t *_Nonnull, zip_source_layered_callback _Nonnull, void *_Nullable, zip_error_t *_Nullable);
-- ZIP_EXTERN zip_int64_t zip_source_make_command_bitmap(zip_source_cmd_t, ...);
{#fun zip_source_open                   { `ZipSource' } -> `CInt' #}
{#fun zip_source_pass_to_lower_layer    { `ZipSource', `Ptr ()', `Word64', `ZipSourceCmd' } -> `Int64' #}
{#fun zip_source_read                   { `ZipSource', `Ptr ()', `Word64' } -> `Int64' #}
{#fun zip_source_rollback_write         { `ZipSource' } -> `()' #}
{#fun zip_source_seek                   { `ZipSource', `Int64', `CInt' } -> `CInt' #}
{#fun zip_source_seek_compute_offset    { `Word64', `Word64', `Ptr ()', `Word64', nullableIn `Maybe ZipError' } -> `Int64' #}
{#fun zip_source_seek_write             { `ZipSource', `Int64', `CInt' } -> `CInt' #}
{#fun zip_source_stat                   { `ZipSource', `ZipStat' } -> `CInt' #}
{#fun zip_source_tell                   { `ZipSource' } -> `Int64' #}
{#fun zip_source_tell_write             { `ZipSource' } -> `Int64' #}
-- #ifdef _WIN32
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32a(zip_t *_Nonnull, const char *_Nonnull, zip_uint64_t, zip_int64_t);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32a_create(const char *_Nonnull, zip_uint64_t, zip_int64_t, zip_error_t *_Nullable);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32handle(zip_t *_Nonnull, void *_Nonnull, zip_uint64_t, zip_int64_t);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32handle_create(void *_Nonnull, zip_uint64_t, zip_int64_t, zip_error_t *_Nullable);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32w(zip_t *_Nonnull, const wchar_t *_Nonnull, zip_uint64_t, zip_int64_t);
-- ZIP_EXTERN zip_source_t *_Nullable zip_source_win32w_create(const wchar_t *_Nonnull, zip_uint64_t, zip_int64_t, zip_error_t *_Nullable);
-- #endif
{#fun zip_source_window_create          { `ZipSource', `Word64', `Int64', nullableIn `Maybe ZipError' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_write                  { `ZipSource', `Ptr ()', `Word64' } -> `Int64' #}
{#fun zip_source_zip_file               { `Zip', `Zip', `Word64', `ZipFlags', `Word64', `Int64', nullableInString* `Maybe B.ByteString' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_source_zip_file_create        { `Zip', `Word64', `ZipFlags', `Word64', `Int64', nullableInString* `Maybe B.ByteString', nullableIn `Maybe ZipError' } -> `Maybe ZipSource' nullableOut #}
{#fun zip_stat                          { `Zip', useAsCString* `B.ByteString', `ZipFlags', `ZipStat' } -> `CInt' #}
{#fun zip_stat_index                    { `Zip', `Word64', `ZipFlags', `ZipStat' } -> `CInt' #}
{#fun zip_stat_init                     { `ZipStat' } -> `()' #}
{#fun zip_strerror                      { `Zip' } -> `B.ByteString' packCString* #}
{#fun zip_unchange                      { `Zip', `Word64' } -> `CInt' #}
{#fun zip_unchange_all                  { `Zip' } -> `CInt' #}
{#fun zip_unchange_archive              { `Zip' } -> `CInt' #}
{#fun zip_compression_method_supported  { `Int32', `CInt' } -> `CInt' #}
{#fun zip_encryption_method_supported   { `Word16', `CInt' } -> `CInt' #}
