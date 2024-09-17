{-# LANGUAGE CApiFFI #-}

module Hostname
( hostname
, hostnameShort
) where

import Data.List.Split (splitOn)

import Foreign.C.Types (CSize (CSize), CInt (CInt), CLong(CLong))
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.C.Error (throwErrno)

foreign import capi "unistd.h gethostname" c_gethostname :: CString -> CSize -> IO CInt
foreign import capi "unistd.h sysconf" c_sysconf :: CInt -> IO CLong
foreign import capi "unistd.h value _SC_HOST_NAME_MAX" _sc_host_name_max :: CInt

hostname :: IO String
hostname = do
    host_name_max <- c_sysconf _sc_host_name_max
    allocaBytes (fromIntegral host_name_max) $ \ptr -> do
        ret <- c_gethostname ptr (fromIntegral host_name_max)
        if ret == 0
            then peekCString ptr
            else throwErrno "hostname"

hostnameShort :: IO String
hostnameShort = head . splitOn '.' <$> hostname
