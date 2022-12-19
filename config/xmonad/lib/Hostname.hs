{-# LANGUAGE CPP #-}

module Hostname (getHostName)
where

#if defined MIN_VERSION_hostname && MIN_VERSION_hostname(1,0,0)
import qualified Network.HostName as HN
#else
import System.Process 
    ( createProcess
    , std_out
    , CreatePipe
    , proc
    )
#endif


-- | Return basename of hostname, found by a suitable method.
getHostName :: IO String

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

#if defined MIN_VERSION_hostname && MIN_VERSION_hostname(1,0,0)
getHostName = fst . splitBy '.' <$> HN.getHostName
#else
getHostName = fst . splitBy '.' . init
            <$> readCreateProcess (proc "hostname" []) ""
-- head . lines <$> readFile "/etc/hostname"
-- hostname
-- hostname -s
-- uname -n
#endif
