{-# LANGUAGE OverloadedStrings #-}

module Volume where

import DBus
import qualified DBus.Client as DBus.Client
import DBus.Client
    ( getProperty
    , getPropertyValue
    , call
    , setProperty
    , connect
    , connectSession
    )
import DBus.Socket (SocketError)
import Data.Maybe (fromJust)
import Data.Either (either)

import Control.Exception

import Data.Word

-- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/

getPath :: DBus.Client.Client -> IO (Either MethodError String)
getPath client =
    getPropertyValue client $ (methodCall "/org/pulseaudio/server_lookup1"
                                "org.PulseAudio.ServerLookup1"
                                "Address")
                             { methodCallDestination = Just "org.PulseAudio1" }
-- Right (Variant "unix:path=/run/user/1000/pulse/dbus-socket")

toMaybe :: Either a b -> Maybe b
toMaybe (Left _)  = Nothing
toMaybe (Right a) = Just a

getAddress :: DBus.Client.Client -> IO (Maybe DBus.Address)
getAddress client = do
    e <- toMaybe <$> getPath client
    return $ parseAddress =<< e

connectPulseDBus :: IO (Maybe DBus.Client.Client)
connectPulseDBus = do
    sessionClient <- connectSession
    e <- getAddress sessionClient
    case e of
        Nothing -> return Nothing
        Just addr -> do
            eConnection <- try (connect addr) :: IO (Either SocketError DBus.Client.Client)
            case eConnection of
                Left _           -> return Nothing
                Right connection -> return . Just $ connection

instance Exception MethodError

throwIfLeft :: Exception a => Either a b -> IO b
throwIfLeft (Left err) = throwIO err
throwIfLeft (Right v) = return v

setMute :: Bool -> ObjectPath -> DBus.Client.Client -> IO ()
setMute value path client = do
    throwIfLeft =<< setProperty client
                     (methodCall path "org.PulseAudio.Core1.Device" "Mute")
                     (toVariant . toVariant $ value)
    return ()

getMute      :: ObjectPath -> DBus.Client.Client -> IO Bool
getMute path client = do
    eth <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Mute"
    fromJust . fromVariant <$> throwIfLeft eth


mute   = setMute True
unmute = setMute False

toggleMute   :: ObjectPath -> DBus.Client.Client -> IO ()
toggleMute p c = getMute p c >>= \x -> setMute (not x) p c

setVolume    :: Word32 -> ObjectPath -> DBus.Client.Client -> IO ()
setVolume v path client = do
    -- list since the sink can have multiple channels.
    -- single values means set all
    throwIfLeft =<< setProperty client
                       (methodCall path "org.PulseAudio.Core1.Device" "Volume")
                       (toVariant (toVariant [ v ]))
    return ()

getVolume    :: ObjectPath -> DBus.Client.Client -> IO [Word32]
getVolume path client = do
    eth <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Volume"
    lst <- fromJust . fromVariant <$> throwIfLeft eth
    return lst

changeVolume :: (Word32 -> Word32) -> ObjectPath -> DBus.Client.Client -> IO Word32
changeVolume f p c = do
    v <- getVolume p c
    let v' = f $ maximum v
    setVolume v' p c
    return v'

-- The fromIntegral's are to ensure we don't get a negative overflow
modVolume :: Integral a => a -> ObjectPath -> DBus.Client.Client -> IO Word32
modVolume change = changeVolume (fromIntegral . max 0 . min (2^16) . (+ change) . fromIntegral)

getSinks :: DBus.Client.Client -> IO [ObjectPath]
getSinks client = do
    sinks <- getProperty client $ methodCall "/org/pulseaudio/core1" "org.PulseAudio.Core1" "Sinks"
    fromJust . fromVariant <$> throwIfLeft sinks

-- Returns names suitable for getSinkByName
getDeviceName :: ObjectPath -> DBus.Client.Client -> IO String
getDeviceName path client = do
    sinks <- getProperty client $ methodCall path "org.PulseAudio.Core1.Device" "Name"
    fromJust . fromVariant <$> throwIfLeft sinks

getSinkByName :: String -> DBus.Client.Client -> IO (Maybe ObjectPath)
getSinkByName name client = do
    eth <- call client $ (methodCall "/org/pulseaudio/core1" "org.PulseAudio.Core1" "GetSinkByName")
                             { methodCallBody = [toVariant name] }
    case eth of
        Right ret -> return . fromVariant . head . methodReturnBody $ ret
        Left  err -> do putStrLn . show $ err
                        return Nothing

listSinks :: DBus.Client.Client -> IO [(ObjectPath, String)]
listSinks client = do
    sinks <- getSinks client
    mapM (\path -> do
            name <- getDeviceName path client
            return (path, name))
        sinks
