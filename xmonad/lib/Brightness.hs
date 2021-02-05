module Brightness 
( updateBrightness
)
where

import System.IO

updateBrightness :: Int -> IO ()
updateBrightness change = do
    let bfile = "/sys/class/backlight/intel_backlight/brightness"
    f <- openFile bfile ReadWriteMode
    current <- read <$> hGetLine f
    hPutStr f (show $ current + change)
    hClose f
    -- current <- read . head . lines <$> readFile bfile
    -- writeFile bfile (show $ current + change)
