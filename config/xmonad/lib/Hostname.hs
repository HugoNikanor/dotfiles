{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Hostname (getHostName)
where

#if defined MIN_VERSION_hostname
    #if ! MIN_VERSION_hostname(1,0,0)
        #error "Hostname library found, but to old version"
    #else
        import qualified Network.HostName as HN
    #endif
#else
import System.Process
    ( readProcess
    , StdStream(CreatePipe)
    , proc
    )
import Control.Exception (try)
import Data.Either (fromRight)
import GHC.Exception.Type
#endif


swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- | Split list once by value, returning the tail along with the head.
-- Use splitBy instead.
-- ==== __Examples__
-- >>> splitBy' '.' "www.example.com"
-- ("example.com", "www")
splitBy' :: Eq a => a -> [a] -> ([a], [a])
splitBy' x [] = ([], [])
splitBy' x (y:ys)
    | x == y    = (ys, [])
    | otherwise = (y:) <$> splitBy' x ys

-- | Split list once by value, returning the head and tail, not
-- including the delimiter.
splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy x = swap . splitBy' x


-- | Return basename of hostname, found by a suitable method.
getHostName :: IO String

#if defined MIN_VERSION_hostname
getHostName = fst . splitBy '.' <$> HN.getHostName
#else
getHostName = do
    -- A apperently need to specify the exception kind here.
    -- SomeException is hopefully some form of "base" exception, since
    -- we want to catch everything.
    eHostname :: Either GHC.Exception.Type.SomeException String
        <- try $ fst . splitBy '.' . init <$> readProcess "hostname" [] []
    case eHostname of
        Right hn -> return hn
        Left _ -> return "unknown"
#endif
