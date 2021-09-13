module Brightness
( updateBrightness
, getBrightness
, hasBacklight
, Backlight ()
)
where

import System.IO
import System.Directory (doesDirectoryExist)

dropRight n = reverse . drop n . reverse

-- Thin wrapper around a /sys/.../backlight path.
-- Not exported, which means that the only way to call
-- {get,update}Brightness is by first calling hasBacklight
newtype Backlight = Backlight String

hasBacklight :: String -> IO (Maybe Backlight)
hasBacklight str = do
    let path = "/sys/class/backlight/" ++ str
    exists <- doesDirectoryExist path
    if exists
        then return . Just . Backlight $ str
        else return Nothing

getBrightness :: Backlight -> IO Double
getBrightness (Backlight path) = do
   current <- read . dropRight 1 <$> (readFile $ path ++ "/actual_brightness")
   max     <- read . dropRight 1 <$> (readFile $ path ++ "/max_brightness")
   return $ current / max


-- explicit file handles to force write.
updateBrightness :: Backlight -> Int -> IO ()
updateBrightness (Backlight path) change = do
    let bfile = path ++ "/brightness"
    f <- openFile bfile ReadWriteMode
    current <- read <$> hGetLine f
    hPutStr f (show $ current + change)
    hClose f
    -- current <- read . head . lines <$> readFile bfile
    -- writeFile bfile (show $ current + change)
