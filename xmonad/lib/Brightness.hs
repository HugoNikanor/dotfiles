module Brightness 
( updateBrightness
, getBrightness
)
where

import System.IO

dropRight n = reverse . drop n . reverse

getBrightness :: IO Double
getBrightness = do
   let path = "/sys/class/backlight/intel_backlight/"
   current <- read . dropRight 1 <$> (readFile $ path ++ "actual_brightness")
   max     <- read . dropRight 1 <$> (readFile $ path ++ "max_brightness")
   return $ current / max

updateBrightness :: Int -> IO ()
updateBrightness change = do
    let bfile = "/sys/class/backlight/intel_backlight/brightness"
    f <- openFile bfile ReadWriteMode
    current <- read <$> hGetLine f
    hPutStr f (show $ current + change)
    hClose f
    -- current <- read . head . lines <$> readFile bfile
    -- writeFile bfile (show $ current + change)
