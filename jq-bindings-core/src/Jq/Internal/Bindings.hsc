{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jq.Internal.Bindings where

import           Foreign (Ptr, Bits)
import           Foreign.C.String
import           Foreign.C.Types

#include "jv.h"
#include "jq.h"

newtype Jv = Jv (Ptr Jv)
newtype JqState = JqState (Ptr JqState)

newtype JvKind = JvKind CInt
  deriving (Eq, Show, Integral, Real, Num, Enum, Ord)

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

-- | Frees both Jvs
foreign import ccall unsafe "wrapper.c jv_identical_w"
  jvIdentical :: Jv -> Jv -> IO CInt

-- | Frees both Jvs
foreign import ccall unsafe "wrapper.c jv_contains_w"
  jvContains :: Jv -> Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_invalid_w"
  jvInvalid :: IO Jv

foreign import ccall unsafe "wrapper.c jv_invalid_get_msg_w"
  jvInvalidGetMsg :: Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_null_w"
  jvNull :: IO Jv

foreign import ccall unsafe "wrapper.c jv_bool_w"
  jvBool :: CInt -> IO Jv

foreign import ccall unsafe "wrapper.c jv_number_w"
  jvNumber :: CDouble -> IO Jv

-- | Does not consume
foreign import ccall unsafe "wrapper.c jv_number_value_w"
  jvNumberValue :: Jv -> IO CDouble

foreign import ccall unsafe "wrapper.c jv_is_integer_w"
  jvIsInteger :: Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_array_w"
  jvArray :: IO Jv

foreign import ccall unsafe "wrapper.c jv_array_sized_w"
  jvArraySized :: CInt -> IO Jv

-- | Consumes
foreign import ccall unsafe "wrapper.c jv_array_length_w"
  jvArrayLength :: Jv -> IO CInt

-- | Consumes input
foreign import ccall unsafe "wrapper.c jv_array_get_w"
  jvArrayGet :: Jv -> CInt -> IO Jv

-- | Does not free the value arg unless the idx is bad
foreign import ccall unsafe "wrapper.c jv_array_set_w"
  jvArraySet :: Jv -> CInt -> Jv -> IO ()

-- | First arg is array, second is a value
foreign import ccall unsafe "wrapper.c jv_array_append_w"
  jvArrayAppend :: Jv -> Jv -> IO ()

-- | first arg is mutated
foreign import ccall unsafe "wrapper.c jv_array_concat_w"
  jvArrayConcat :: Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_array_slice_w"
  jvArraySlice :: Jv -> CInt -> CInt -> IO Jv

foreign import ccall unsafe "wrapper.c jv_array_indexes_w"
  jvArrayIndexes :: Jv -> Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_value_w"
  jvStringValue :: Jv -> IO CString

foreign import ccall unsafe "wrapper.c jv_string_w"
  jvString :: CString -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_sized_w"
  jvStringSized :: CString -> CInt -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_length_bytes_w"
  jvStringLengthBytes :: Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_string_length_codepoints_w"
  jvStringLengthCodepoints :: Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_string_indexes_w"
  jvStringIndexes :: Jv -> Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_string_slice_w"
  jvStringSlice :: Jv -> CInt -> CInt -> IO Jv

foreign import ccall unsafe "wrapper.c jv_object_w"
  jvObject :: IO Jv

foreign import ccall unsafe "wrapper.c jv_object_get_w"
  jvObjectGet :: Jv -> Jv -> IO Jv

foreign import ccall unsafe "wrapper.c jv_object_has_w"
  jvObjectHas :: Jv -> Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_object_set_w"
  jvObjectSet :: Jv -> Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_object_delete_w"
  jvObjectDelete :: Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_object_length_w"
  jvObjectLength :: Jv -> IO CInt

foreign import ccall unsafe "wrapper.c jv_object_merge_w"
  jvObjectMerge :: Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_object_merge_recursive_w"
  jvObjectMergeRecursive :: Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_setpath_w"
  jvSetpath :: Jv -> Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_getpath_w"
  jvGetpath :: Jv -> Jv -> IO ()

foreign import ccall unsafe "wrapper.c jv_parse_w"
  jvParse :: CString -> IO Jv

foreign import ccall unsafe "wrapper.c jv_load_file_w"
  jvLoadFile :: CString -> CInt -> IO Jv

-- | Frees the Jv
foreign import ccall unsafe "wrapper.c jv_dump_string_w"
  jvDumpString :: Jv -> JvPrintFlags -> IO Jv

foreign import ccall unsafe "wrapper.c jv_keys_w"
  jvKeys :: Jv -> IO Jv

--------------------------------------------------------------------------------
-- JQ programs
--------------------------------------------------------------------------------

foreign import ccall unsafe "jq.h jq_init"
  jqInit :: IO JqState

foreign import ccall unsafe "jq.h jq_compile"
  jqCompile :: JqState -> CString -> IO CInt

foreign import ccall unsafe "wrapper.c jq_start_w"
  jqStart :: JqState -> Jv -> CInt -> IO ()

foreign import ccall unsafe "wrapper.c jq_next_w"
  jqNext :: JqState -> IO Jv

foreign import ccall unsafe "wrapper.c jq_teardown_w"
  jqTeardown :: JqState -> IO ()
