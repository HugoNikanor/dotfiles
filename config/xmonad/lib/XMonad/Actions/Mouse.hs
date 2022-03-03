
getMousePos :: IO (Int, Int)
getMousePos = (\[a, b] -> (a, b)) . map (read . (drop 2)) . take 2 . words <$> readProcess "xdotool" ["getmouselocation"] ""

{-
getMousePos' :: X (Int, Int)
getMousePos' = io getMousePos''
-}


normalize :: Rational -> Rational
normalize = toRational . abs . (/ (pi/2)) . atan . fromRational

normalizedMouse :: IO (Rational, Rational)
normalizedMouse = getMousePos >>= \(x, y) -> return (f x, f y)
    where f = normalize . toRational

getMousePos' :: Query (Rational, Rational)
getMousePos' = liftX . io $ normalizedMouse

coords :: Query (Rational, Rational)
coords = return (1/2, 1/2)


-- ManageHook == Query (Endo WindowSet)
{-
doMoveToMouse :: ManageHook
doMoveToMouse = do
        (x, y) <- getMousePos
        --let move (W.RationalRect _ _ w h) = W.RationalRect x y w h
        --odoFloatDep move
        doFloat
        --doFloatAt (x/1920) (y/1080)
-}

doMoveToMouse :: ManageHook
doMoveToMouse = do
    --(x, y) <- getMousePos
    --let (x, y) = (1/10, 2/10)
    --(return (0, 0)) >>= (\(x, y) -> doFloatAt x y)
    --doFloatAt 0 0
    --getMousePos >>= (\(x, y) -> doFloatAt x y)
    --getMousePos >>= (\(x, y) -> doFloatAt x y)
    --getMousePos'
    liftX . io $ appendFile "/home/hugo/test" "test\n"
    -- this kinda crashes, and prematurely returns from
    -- the function with a "default" manage hook.
    (x, y) <- liftX . io $ normalizedMouse --getMousePos'
    let s = show x ++ show y ++ "\n"
    liftX . io $ appendFile "/home/hugo/test" s
    doFloatAt 0 0
    -- liftX . io $ normalizedMouse >>= uncurry doFloat

