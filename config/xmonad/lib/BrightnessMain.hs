{-# LANGUAGE ScopedTypeVariables #-}

import Brightness
import System.Exit
import System.Environment (getArgs)

main = do
    args <- getArgs
    let change :: Int = read $ args !! 0
    bl' <- hasBacklight "intel_backlight"
    bl <- case bl' of 
        Just bl -> return bl
        Nothing -> exitFailure
    updateBrightness bl change
    current <- getBrightness bl
    putStrLn $ "Current = " ++ show current
