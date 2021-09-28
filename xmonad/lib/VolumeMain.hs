{-# LANGUAGE OverloadedStrings #-}

import DBus (formatObjectPath)
import Data.Maybe (fromJust, listToMaybe)

import Volume

import DBus
import DBus.Client

listSinks' :: Client -> IO ()
listSinks' client = do
    putStrLn "Available sinks:"
    sinks <- listSinks client
    mapM_ (\(path, name) -> let p = formatObjectPath path
                            in putStrLn $ "• " ++ p ++ " - " ++ name)
        sinks

listenForChanges :: Client -> IO ()
listenForChanges client = do

    -- Our DBus library provides the method "addMatch", which makes
    -- the DBus call "AddMatch". Unfortunately, Pulseaudio doesn't
    -- implement that call, and instead opts for "ListenForSignal",
    -- meaning that we have to implement stuff ourselves.
    --
    -- TODO do this
    --
    -- Ideally we would want to listen of "VolumeUpdate".
    --
    -- https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Core/

    addMatch client (matchAny { matchPath = Just "/org/pulseaudio/core1/sink1"} ) (\signal -> do 
        putStr "signal: "
        putStrLn . show $ signal)
    -- sinks !! 0
    return ()

main :: IO ()
main = do
    client <- fromJust <$> connectPulseDBus

    listSinks' client

    return ()


