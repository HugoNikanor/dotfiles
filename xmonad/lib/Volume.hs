{-# LANGUAGE OverloadedStrings #-}

module Volume where

import DBus
import DBus.Client
import Data.Maybe (fromJust)
import Data.Either (either)

import Control.Exception

import Data.Word

-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/

getPath client =
    getPropertyValue client $ (methodCall "/org/pulseaudio/server_lookup1"
                                "org.PulseAudio.ServerLookup1"
                                "Address")
                             { methodCallDestination = Just "org.PulseAudio1" }
-- Right (Variant "unix:path=/run/user/1000/pulse/dbus-socket")

connectPulseDBus :: IO (Maybe Client)
connectPulseDBus = do
    sessionClient <- connectSession
    e <- getPath sessionClient
    case e of
        Left _ -> return Nothing
        Right v -> case parseAddress v of
            Nothing -> return Nothing
            Just v -> Just <$> connect v

instance Exception MethodError

throwIfLeft :: Exception a => Either a b -> IO b
throwIfLeft (Left err) = throwIO err
throwIfLeft (Right v) = return v

setMute :: Bool -> ObjectPath -> Client -> IO ()
setMute value path client = do
    throwIfLeft =<< setProperty client
                     (methodCall path "org.PulseAudio.Core1.Device" "Mute")
                     (toVariant . toVariant $ value)
    return ()

getMute      :: ObjectPath -> Client -> IO Bool
getMute path client = do
    eth <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Mute"
    fromJust . fromVariant <$> throwIfLeft eth


mute   = setMute True
unmute = setMute False

toggleMute   :: ObjectPath -> Client -> IO ()
toggleMute p c = getMute p c >>= \x -> setMute (not x) p c

setVolume    :: Word32 -> ObjectPath -> Client -> IO ()
setVolume v path client = do
    -- list since the sink can have multiple channels.
    -- single values means set all
    throwIfLeft =<< setProperty client
                       (methodCall path "org.PulseAudio.Core1.Device" "Volume")
                       (toVariant (toVariant [ v ]))
    return ()

getVolume    :: ObjectPath -> Client -> IO [Word32]
getVolume path client = do
    eth <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Volume"
    lst <- fromJust . fromVariant <$> throwIfLeft eth
    return lst

changeVolume :: (Word32 -> Word32) -> ObjectPath -> Client -> IO Word32
changeVolume f p c = do
    v <- getVolume p c
    let v' = f $ maximum v
    setVolume v' p c
    return v'

-- The fromIntegral's are to ensure we don't get a negative overflow
modVolume :: Integral a => a -> ObjectPath -> Client -> IO Word32
modVolume change = changeVolume (fromIntegral . max 0 . min (2^16) . (+ change) . fromIntegral)

getSinks :: Client -> IO [ObjectPath]
getSinks client = do
    sinks <- getProperty client $ methodCall "/org/pulseaudio/core1" "org.PulseAudio.Core1" "Sinks"
    fromJust . fromVariant <$> throwIfLeft sinks

-- Returns names suitable for getSinkByName
getDeviceName :: ObjectPath -> Client -> IO String
getDeviceName path client = do
    sinks <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Name"
    fromJust . fromVariant <$> throwIfLeft sinks

getSinkByName :: String -> Client -> IO (Maybe ObjectPath)
getSinkByName name client = do
    eth <- call client $ (methodCall "/org/pulseaudio/core1" "org.PulseAudio.Core1" "GetSinkByName")
                             { methodCallBody = [toVariant name] }
    case eth of
        Right ret -> return . fromVariant . head . methodReturnBody $ ret
        Left  err -> do putStrLn . show $ err
                        return Nothing
