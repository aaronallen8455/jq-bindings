{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jq.Internal.Bindings where

import           Foreign (Ptr, Bits)
import           Foreign.C.String
import           Foreign.C.Types

#include "jv.h"

newtype Jv = Jv (Ptr Jv)

newtype JvKind = JvKind CInt
  deriving Eq

newtype JvPrintFlags = JvPrintFlags CInt
  deriving (Eq, Bits, Num)

#{ enum JvKind, JvKind
 , jvKindInvalid = JV_KIND_INVALID
 , jvKindNull    = JV_KIND_NULL
 , jvKindFalse   = JV_KIND_FALSE
 , jvKindTrue    = JV_KIND_TRUE
 , jvKindNumber  = JV_KIND_NUMBER
 , jvKindString  = JV_KIND_STRING
 , jvKindArray   = JV_KIND_ARRAY
 , jvKindObject  = JV_KIND_OBJECT
 }

#{ enum JvPrintFlags, JvPrintFlags
 , jvPrintPretty   = JV_PRINT_PRETTY
 , jvPrintAscII    = JV_PRINT_ASCII
 , jvPrintColor    = JV_PRINT_COLOR
 , jvPrintColour   = JV_PRINT_COLOUR
 , jvPrintSorted   = JV_PRINT_SORTED
 , jvPrintInvalid  = JV_PRINT_INVALID
 , jvPrintRefCount = JV_PRINT_REFCOUNT
 , jvPrintTab      = JV_PRINT_TAB
 , jvPrintIsatty   = JV_PRINT_ISATTY
 , jvPrintSpace0   = JV_PRINT_SPACE0
 , jvPrintSpace1   = JV_PRINT_SPACE1
 , jvPrintSpace2   = JV_PRINT_SPACE2
 }

foreign import ccall unsafe "wrapper.c jv_get_kind_w"
  jvGetKind :: Jv -> IO JvKind

foreign import ccall unsafe "jv.h jv_kind_name"
  jvKindName :: JvKind -> IO CString

foreign import ccall unsafe "wrapper.c jv_copy_w"
  jvCopy :: Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_free_w"
  jvFree :: Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_get_refcnt_w"
  jvGetRefcnt :: Jv -> IO CInt

-- | Frees both Jvs
foreign import ccall unsafe "wrapper.c jv_equal_w"
  jvEqual :: Jv -> Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_identical_w"
  jvIdentical :: Jv -> Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_contains_w"
  jvContains :: Jv -> Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_invalid_w"
  jvInvalid :: IO Jv

foreign import ccall unsafe "wrapper.c jv_invalid_with_msg_w"
  jvInvalidWithMsg :: Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_invalid_get_msg_w"
  jvInvalidGetMsg :: Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_invalid_has_msg_w"
  jvInvalidHasMsg :: Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_null_w"
  jvNull :: IO Jv

foreign import ccall unsafe "wrapper.c jv_true_w"
  jvTrue :: IO Jv

foreign import ccall unsafe "wrapper.c jv_false_w"
  jvFalse :: IO Jv

foreign import ccall unsafe "wrapper.c jv_bool_w"
  jvBool :: CInt -> IO Jv

--------------------------------------------------------------------------------

foreign import ccall unsafe "wrapper.c jv_parse_w"
  jvParse :: CString -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_w"
  jvString :: CString -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_value_w"
  jvStringValue :: Jv -> IO CString

-- | Frees the Jv
foreign import ccall unsafe "wrapper.c jv_dump_string_w"
  jvDumpString :: Jv -> JvPrintFlags -> IO Jv
