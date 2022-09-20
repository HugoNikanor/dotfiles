> {-# LANGUAGE CPP #-}
>
> module Config
> ( xmain
> ) where

#ifndef MIN_VERSION_xmonad_contrib
#error XMonad contrib required for building.
#endif

> import System.IO (hPutStrLn)
> import System.Environment (setEnv)
> import System.Locale (defaultTimeLocale, TimeLocale(wDays))
> import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
> import System.Directory (doesPathExist, findExecutable)
> import Data.List (isInfixOf)
> import Data.Function (fix)
> import Data.Foldable (toList)
> import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
> import Data.Functor ((<&>))

> import Text.Printf (printf)

Provides symbol names for all weird X keycodes. So most things
starting with xF86 in this file.

This is the analoge of =/usr/include/X11/XF86keysym.h=.

> import Graphics.X11.ExtraTypes.XF86

> import XMonad

> import XMonad.Actions.PhysicalScreens
>     (viewScreen, PhysicalScreen (P), sendToScreen)
> import XMonad.Actions.DynamicWorkspaces
>     ( selectWorkspace
>     , removeWorkspace
>     , addWorkspacePrompt
>     , withWorkspace
>     , renameWorkspace )
> import XMonad.Actions.CycleWS (toggleWS)
> import XMonad.Actions.Warp (banish, warpToScreen, Corner (LowerRight))
> import XMonad.Actions.Navigation2D (windowGo, windowSwap)
> import XMonad.Actions.GridSelect (gs_navigate)
> import XMonad.Actions.GridSelect as GS
> import XMonad.Actions.Submap (submap)

> import XMonad.Hooks.InsertPosition
>     (Position (Below), Focus (Newer), insertPosition)
> import XMonad.Hooks.ManageHelpers (isDialog)
> import XMonad.Hooks.Place
> import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docks)
> import XMonad.Hooks.DynamicLog
>     ( xmobarColor
>     , xmobarPP
>     , dynamicLogWithPP
>     , ppCurrent
>     , ppTitle
>     , ppVisible
>     , ppHidden
>     , ppWsSep
>     , ppSep
>     , ppOutput
>     , ppExtras
>     , dzenPP
>     , dzenColor
>     , shorten)
> import XMonad.Hooks.EwmhDesktops (ewmh{-, ewmhDesktopEventHook, ActiveWindow-})

> import XMonad.Layout(Mirror (Mirror))

> import XMonad.Layout.Grid (Grid (Grid, GridRatio))
> import XMonad.Layout.PerScreen (ifWider)
> import XMonad.Layout.Dishes (Dishes (Dishes))
> import XMonad.Layout.Decoration (shrinkText)
> import XMonad.Layout.DwmStyle (dwmStyle)
> import XMonad.Layout.OneBig (OneBig (OneBig))
> import XMonad.Layout.Spiral (spiral)
> import XMonad.Layout.WindowNavigation (windowNavigation)
> import XMonad.Layout.MultiColumns (multiCol)
> import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol))
> import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle1)
> import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))

IndependentScreens is the library which should allow things
to happen at points not currently in focus.

> import XMonad.Layout.IndependentScreens (countScreens)

> import XMonad.Prompt (XPConfig (..), XPPosition (Top))
> import XMonad.Prompt.Input (inputPrompt)
> import XMonad.Prompt.Shell (shellPrompt)
> import XMonad.Prompt.XMonad (xmonadPrompt)

> import XMonad.Util.Types (Direction2D (U, D, L, R))
> import XMonad.Util.Run (spawnPipe)
> import qualified XMonad.Util.Loggers as Logger
> import XMonad.Util.Scratchpad
>   ( scratchpadManageHook
>   , scratchpadSpawnActionCustom
>   )

> import qualified Data.Map as M
> import qualified XMonad.StackSet as W
> import qualified XMonad.Layout.BoringWindows as B
> import qualified XMonad.Layout.SubLayouts as S

> import Brightness
#ifdef MIN_VERSION_dbus
> import Volume
> import DBus (ObjectPath)
> import qualified DBus.Client as DBus
#endif



> type KeyBind = (KeyMask, KeySym)
> type KeyAction = (KeyBind, X ())



> dropRight n = reverse . drop n . reverse



The X key names of the swedish letters is

> xK_å = xK_aring
> xK_ä = xK_adiaeresis
> xK_ö = xK_odiaeresis



Borrowed from darcs. Warps the mouse to the center of the current
(physical) monitor.

> warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

gs_navigate : TwoD a (Maybe a)
makeXEventHandler : ((KeySym, String, KeyMask) -> TwoD a (Maybe a)) -> TwoD a (Maybe a)
shadowWithKeymap : Map KeyBind a -> ((KeySym, String, KeyMask) -> a) -> (KeySym, String, KeyMask) -> a

It would be really nice if these showed previews of the windows,
for terminals displayed the last command run, and also display
which workspace the window resides in.

> gsConfig :: GSConfig Window
> gsConfig = def { gs_navigate = fix $ \self ->
>     let navKeyMap = M.mapKeys ((,) 0) $ M.fromList $
>                 [(xK_Escape, GS.cancel)
>                 ,(xK_Return, GS.select)
>                 ,(xK_slash , GS.substringSearch self)]
>            ++
>             map (\(k,a) -> (k,a >> self))
>                 [(xK_Left  , GS.move (-1,0 ))
>                 ,(xK_h     , GS.move (-1,0 ))
>                 ,(xK_Right , GS.move (1,0  ))
>                 ,(xK_l     , GS.move (1,0  ))
>                 ,(xK_Down  , GS.move (0,1  ))
>                 ,(xK_j     , GS.move (0,1  ))
>                 ,(xK_Up    , GS.move (0,-1 ))
>                 ,(xK_k     , GS.move (0,-1 ))
>                 ,(xK_space , GS.setPos (0,0))
>                 ]
>     in makeXEventhandler $ shadowWithKeymap navKeyMap (const self) }



TODO
====
* Possibly add scratchpad (floating term) (I have that for gvim now!)
* If only one program is in a workspace the workspace should change name to that

> myFont = "Fira Mono"

TODO figure out how to run this from main

> spawnToWorkspace :: String -> String -> X ()
> spawnToWorkspace workspace program = do
>     spawn program
>     windows $ W.greedyView workspace


These should have a propper place somewhere.

> restartXMonad = spawn "xmonad --recompile; xmonad --restart"

> myXPConfig = def
>   { position = Top -- CenteredAt
>   , historySize = 100
>   , autoComplete = Just 1
>   -- , font = myFont
>   -- , bgColor = "black"
>   -- , searchPredicate = isInfixOf
>   , bgColor = bgColor'
>   , fgColor = fgColor'
>   , fgHLight = "yellow"
>   , bgHLight = bgColor'
>   , promptBorderWidth = 0
>   }

> -- decoration = dwmStyle shrinkText def

The window layouts are split into two parts, those for wide
screens and those for tall screens. Currently I only check
a `tallThreshold', but this obviously fails for higher DPI
screens.

> myLayouts = mkToggle1 FULL wideLayouts {- ifWider tallThreshold wideLayouts tallLayouts -}
>   where tallThreshold = 1200
>         wideLayouts
>                     = Tall 1 (3/100) (3/5)
>                   ||| ThreeCol 1 (3/100) (1/3)
>                   ||| Mirror (multiCol [1, 1, 0] 4 0.01 0.5)
>         tallLayouts = Dishes 1 (1/4)
>                   ||| GridRatio (4/3)

Workspace ID is just the name of the workspace.

> getCurrentWorkspaceId :: X WorkspaceId
> getCurrentWorkspaceId = gets (W.currentTag . windowset)



Keys are currently bound in two sepparate places. Both here,
where I have complete controll over Haskell, and below where
I allow EZConfig to do the work. The settings that are set
here is either due to them requiring some data sent to them
in unusual ways (such as =XMonad.terminal=), or depend on
actuall code (see monitor movement bindings).

Footnote:
  The following section uses the function
=bindWithAndWithoutShift= a fair bit. The function has a
type signature as follows, and is defined in the appendix.

> bindWithAndWithoutShift :: (a -> X ())
>                         -> (a -> X ())
>                         -> [(KeySym, a)]
>                         -> XConfig l
>                         -> [KeyAction]



Monitors should be bound to the keys corresponding to the
keys occupied by the middle three fingers on the left
homerow, togheter with the three keys above that. On Swedish
Dvorak that corresponds to (where `e' is the primary
monitor):

    ä, ö, p
    o, e, u

Hopefully I can get these to be automaticly bound,
unfortunately XMonad uses Xinerama to enumerate monitors,
and I don't understand how they work. It's therefore by the
user required to modifiy the below bindings to match the
actuall numbers of the monitors.

Footnote:
Choose one of these, depending on the current monitor setup.
(`x' declared as binding for /no monitor/)

> x = -1 :: Int

> (a, b, c) = (0, 1, 2)
> (d, e, f) = (3, 4, 5)

> monitorKeys = bindWithAndWithoutShift
>   (\i -> (view . P $ i) >> banish LowerRight)
>   (send . P)
>   [ (xK_ä, a), (xK_comma,  a) -- Double bindings for both swedish
>   , (xK_ö, b), (xK_period, b) -- and american dvorak keyboards.
>   -- , (xK_p, c)
>   , (xK_o, d)
>   , (xK_e, e)
>   , (xK_u, f) ]
>   where
#if MIN_VERSION_xmonad_contrib(0,14,0)
>     view = viewScreen def
>     send = sendToScreen def
#else
>     view = viewScreen
>     send = sendToScreen
#endif



This adds directional movement, instead of the default 1D movement.
Especially good on larger screens.

> movementKeys = bindWithAndWithoutShift
>   (\d -> windowGo d False >> banish LowerRight)
>   (`windowSwap` False)
>   [ (xK_h, L)
>   , (xK_j, D)
>   , (xK_k, U)
>   , (xK_l, R) ]



> data Redraw = Redraw deriving Typeable
> instance Message Redraw

> spaceSubmap :: XConfig a -> M.Map KeyBind (X ())
> spaceSubmap conf@XConfig {XMonad.modMask = modm} = M.fromList $

Directions are inverted to what is "normal". This since I want to think about
it like a lasso going out in that direction.

>       [ ((0, k), sendMessage $ S.pullGroup d)
>       | (k, d) <- [ (xK_j, D)
>                   , (xK_k, U)
>                   , (xK_h, R)
>                   , (xK_l, L) ] ]
>       ++
>       [ ((0, k), action)
>       | (k, action)
>           <- [ (xK_m    , withFocused $ sendMessage . S.MergeAll)
>              , (xK_u    , withFocused $ sendMessage . S.UnMerge)
>              , (xK_space, selectWorkspace myXPConfig)
>              , (xK_d    , removeWorkspace)
>              , (xK_a    , addWorkspacePrompt myXPConfig)
>              , (xK_s    , withWorkspace myXPConfig $ windows . W.shift)
>              , (xK_r    , renameWorkspace myXPConfig { autoComplete = Nothing })
>           ]
>       ]
>       ++
>       [ ((modm, xK_space), selectWorkspace myXPConfig)
>   -- , ("M-<Space> S-s", inputPromptWithCompl myXPConfig "Shift and go" >>= shiftAndGo)
>   -- , ("M-<Space> M-d", removeEmptyWorkspace)
>       ]

Keybinds for flipping the monitor upside down.
Only enabled on single screen setups, since it turns off every screen
except the primary, and since it's only really useful on laptops.

> monitorFlipKeys :: Int -> XConfig l -> [KeyAction]
> monitorFlipKeys 1 conf@XConfig {XMonad.modMask = modm} =
>     [ ((modm, xK_Up)   , spawn "xrandr -o inverted")
>     , ((modm, xK_Down) , spawn "xrandr -o normal") ]
> monitorFlipKeys _ _ = []
>

> otherKeys :: XConfig l -> [KeyAction]
> otherKeys conf@XConfig {XMonad.modMask = modm} =
>     [ ms xK_Return  $ spawn $ XMonad.terminal conf
>     , m  xK_Return  $ windows W.swapMaster

Same bindings again, since my laptop' enter key is apparently (?) the
numpad enter...

>     , ms xK_KP_Enter  $ spawn $ XMonad.terminal conf
>     , m  xK_KP_Enter  $ windows W.swapMaster

>     , m  xK_Tab     $ S.onGroup W.focusDown'
>     , ms xK_Tab     $ S.onGroup W.focusUp'
>     , m  xK_t       $ withFocused $ windows . W.sink
>     , m  xK_f       $ sendMessage $ Toggle FULL
>     , m  xK_n       $ sendMessage NextLayout
>
>     , m  xK_j         B.focusDown
>     , m  xK_k         B.focusUp
>     , ms xK_j       $ windows W.swapDown
>     , ms xK_k       $ windows W.swapUp
>
>     , m  xK_m       $ sendMessage Shrink
>     , m  xK_w       $ sendMessage Expand
>     , ms xK_m       $ sendMessage $ IncMasterN    1
>     , ms xK_w       $ sendMessage $ IncMasterN $ -1
>
>     , m  xK_l       $ S.onGroup W.focusDown'
>     , m  xK_h       $ S.onGroup W.focusUp'
>
>     , ms xK_c         kill
>
>     , m  xK_s         toggleWS
>     , m  xK_g       $ spawn "rofi -show window -show-icons"
>     , ms xK_p       $ shellPrompt myXPConfig { autoComplete = Nothing
>                                              , searchPredicate = isInfixOf }
>     , m  xK_p       $ spawn "rofi -show drun -show-icons"
>     , m  xK_x       $ xmonadPrompt myXPConfig { autoComplete = Nothing }
>     , m  xK_y       $ spawn "passmenu"
>     , m  xK_q         restartXMonad
>
>     , ms xK_f       $ scratchpadSpawnActionCustom "dolphin --name scratchpad"
>
>     , m  xK_space   $ submap $ spaceSubmap conf

>
>     ] where m x op = ((modm, x), op)
>             ms x op = ((modm .|. shiftMask, x), op)



`placeHook' places floating windows at interesting places.
This tries to place windows close to the center of the
screen, which is what I want.

> placeNearCenter = placeHook (withGaps (10, 10, 10, 10) (smart (0.5,0.5)))

Compositioning of rules go from right to left (I think), the
`doFloat' is just so that the window can actually be placeded
smartly after.

> doSmartFloat = placeNearCenter <+> doFloat

> -- "Steam - News (1 of 2)"

> myManageHook = composeAll
>     [ isDialog                --> doSmartFloat
>     , className =? "Gvim"     --> doSmartFloat
>     , className =? "Pinentry" --> doSmartFloat
>     , className =? "pinentry-qt" --> doSmartFloat
>     , className =? "Floating" --> doSmartFloat
>     , className =? "VirtualBox" --> doSmartFloat
>     , className =? "Gimp" --> doShift "gimp"
>     , className =? "Steam" --> doShift "steam"
>     , className =? "Xmessage" --> doSmartFloat
>     , className =? "Gimp"     --> doShift "gimp"
>     , className =? "Steam"    --> doShift "steam"
>     , className =? "vlc"      --> doShift "video"
>     , className =? "lxsession-logout" --> doSmartFloat
>     , className =? "Minecraft 1.12.2" --> doSmartFloat
>     -- this allows the "notification popup" to be placed correctly.
>     -- it does however still steal focus.
>     , title =? "Microsoft Teams Notification" --> doIgnore

WM_NAME(STRING) =
_NET_WM_NAME(UTF8_STRING) = "zoom"

>     , title =? "zoom" --> doFloat
>     -- , (className =? "teams") <> (title =? "") --> doIgnore
>     , className =? "teams" --> doIgnore
>     -- , className =? "VirtualBox Machine" --> doIgnore
>
>     , title     =? "Toolbox"  --> doSmartFloat -- gimp toolbox
>     , title =? "Steam Controller Configurator" --> doFloat
>     , title =? "Python Turtle Graphics" --> doFloat
>     -- Who doesn't this work!
>     -- , className =? "Gimp" <&&> (appName =? "Toolbox" <||> title =? "Toolbox") --> doFloat ]
>     --, className =? "Gimp" --> doFloat ]
>     ]

> shiftAndGo :: Maybe String -> X ()
> shiftAndGo Nothing = return ()
> shiftAndGo (Just str) =
>     windows $ do
>         W.shift str
>         W.greedyView str



Color config borrowed from my Termite config .

> foreground      = "#c5c8c6"
> foreground_bold = "#c5c8c6"
> cursor          = "#c5c8c6"
> background      = "#1d1f21"

> fgColor' = foreground
> bgColor' = "black"

> dzenIcon :: FilePath -> String
> dzenIcon icon = "^i(/home/hugo/.local/share/dzen2/" ++ icon ++ ".xbm)"

> dzenFg :: String -> String
> dzenFg c = "^fg(" ++ c ++ ")"

> dzenPos :: (Show a, Integral a) => a -> String
> dzenPos p = "^p(" ++ show p ++ ")"

> data DzenPosSpecial = LOCK_X
>                     | UNLOCK_X
>                     | LEFT
>                     | RIGHT
>                     | TOP
>                     | CENTER
>                     | BOTTOM
>                     deriving (Enum, Show)

> dzenMove :: DzenPosSpecial -> String
> dzenMove s = "^p(_" ++ show s ++ ")"

> dzenRect :: (Show a, Integral a) => a -> a -> String
> dzenRect width height = "^r(" ++ show width ++ "x" ++ show height ++ ")"

> dzenCircle :: (Show a, Integral a) => a -> String
> dzenCircle r = "^c(" ++ show r ++ ")"
>
> dzenCircle' :: (Show a, Integral a) => a -> String
> dzenCircle' r = "^co(" ++ show r ++ ")"

> formatBatteryDzen :: String -> Int -> String
> -- TODO dzenIcon icon
> formatBatteryDzen icon n = concat [ color n, dzenIcon "battery", " " , show n, "%" ]
>   where
>     color n
>         | n < 10    = dzenFg "red"
>         | n < 60    = dzenFg "yellow"
>         | otherwise = dzenFg "green"


> loggerIfFile :: FilePath -> Logger.Logger -> Logger.Logger
> loggerIfFile path logger = do
>     exists <- io (doesPathExist path)
>     if exists
>       then logger
>       else return Nothing

> roundNearest :: Int -> Int -> Int
> roundNearest target n = target * (n `div` target)

>        -- https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power
> batteryStatusInfix :: String -> String
> batteryStatusInfix "Charging"     = "-charging"
> batteryStatusInfix "Discharging"  = ""
> batteryStatusInfix "Unknown"      = "-missing"
> batteryStatusInfix "Not charging" = ""
> batteryStatusInfix "Full"         = ""

> batteryStatusMost :: Int -> String -> String
> batteryStatusMost _ "Unknown" = "battery-missing"
> batteryStatus percentage status = "battery-level-"
>               ++ (show . roundNearest 10 $ percentage)
>               ++ batteryStatusInfix status

> battery :: FilePath -> Logger.Logger
> battery supply = do
>     let prefix = "/sys/class/power_supply/" ++ supply
>     let path = prefix ++ "/capacity"
>     loggerIfFile path $ do
>        percentage <- read . dropRight 1 <$> io (readFile path)
>        status <- dropRight 1 <$> io (readFile $ prefix ++ "/status")
>        let batIcon = batteryStatusMost percentage status ++ "-symbolic.symbolic.xpm"
>        return . Just . formatBatteryDzen batIcon $ percentage

> dzenSlider :: (RealFrac a) => Integer -> Integer -> String -> a -> String
> dzenSlider width radius icon value =
>   let w = fromIntegral width
>       half_radi = fromIntegral . floor $ fromIntegral radius / 2
>       percentage = floor $ value * w - half_radi
>       -- slide_base = printf "^r(%i)^p(-%i)^p(%i)" width width percentage
>   in dzenRect width 1
>   ++ (dzenPos $ - width)
>   ++ dzenPos percentage
>   ++ icon
>   ++ dzenPos (width - percentage - 16) -- 16 == width of icon (approx.)

> slider = dzenSlider 100 16

TODO put a ^fg(red) before the slider when redshift is activated

> brightness :: Backlight -> Logger.Logger
> brightness backlight = do
>   bright <- io (getBrightness backlight)
>   return . Just $ slider (dzenIcon "brightness") bright

#ifdef MIN_VERSION_dbus
> volume :: ObjectPath -> DBus.Client -> Logger.Logger
> volume p c = do
>   volume <- io (getVolume p c)
>   muted  <- io (getMute p c)
>   let volume' = fromIntegral (maximum volume) / 2^16
>   let slid = slider (dzenIcon "music") $ volume'
>   let mut = if muted then dzenFg "#2f4f4f" else ""
>   return . Just $ mut ++ slid
#endif

> swap :: Logger.Logger
> swap = do
>   alist <- io $ fmap words <$> lines <$> readFile "/proc/meminfo"
>   let [_, total', totalSuff] = head $ filter ((== "SwapTotal:") . head) alist
>   let [_, free', freeSuff]   = head $ filter ((== "SwapFree:")  . head) alist
>   let unused = read free' / read total'
>   let used = (1.0 :: Float) - used
>   return . Just $ printf "%.1f%%" (used * 100)

Log hook borrowed from https://pastebin.com/Pt8LCprY.

> colorFunc = dzenColor
> funcPP = dzenPP
> -- colorFunc = xmobarColor
> -- funcPP = xmobarPP

Sets names of week days to Swedish, for output, should ideally also
set month names.

> timeLocale = defaultTimeLocale {
>   wDays = [ ("Söndag", "Sön")
>           , ("Måndag", "Mån")
>           , ("Tisdag", "Tis")
>           , ("Onsdag", "Ons")
>           , ("Torsag", "Tor")
>           , ("Fredag", "Fre")
>           , ("Lördag", "Lör") ]
> }

Date logger taken from default example loggens, only changed to take
my timeLocale, so I can *finally* get Swedish names!
https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/src/XMonad.Util.Loggers.html#date

> date :: String -> Logger.Logger
> date fmt = io $ do cal <- toCalendarTime =<< getClockTime
>                    return . Just $ formatCalendarTime timeLocale fmt cal


https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html

> myLogHook handle extras = dynamicLogWithPP $ funcPP
>   { ppCurrent = \str -> colorFunc bgColor' "yellow" $ " " ++ str ++ " "
>   , ppVisible = \name -> colorFunc bgColor' "white" $ " " ++ name ++ " "
>   , ppHidden = \name -> colorFunc bgColor' "grey" $ " " ++ name ++ " "
>   , ppTitle = shorten 100
>   , ppWsSep = " "
>
>   , ppSep = dzenFg "" ++ " | "
>   , ppOutput = hPutStrLn handle


https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-Loggers.html

>   , ppExtras = extras
>   }



> xmproc :: String -> String
>     -- Setting an -y value breaks -dock, also, -dock is undocumented?
> xmproc "gandalf" = "dzen2 -fn 'Roboto' -w 1920 -x 1920 -ta l -dock"
> xmproc _         = "dzen2 -fn 'Fira Mono' -ta l -dock"

nameMatches is used when finding proper PulseAudio sinks for the volume slider.
However, since the current first alternative currently works for all
my machines (and since regex libraries are "interesting" in Haskell)
we just return a constant True here.

> nameMatches :: String -> String -> Bool
> nameMatches pattern str = True

> xmain = do
>     hostname <- head . lines <$> readFile "/etc/hostname"
>     setEnv "_JAVA_AWT_WM_NOREPARENTING" "1"
>     nScreens    <- countScreens

set fallback sequnece for terminal emulators

>     termCommand <- head . catMaybes <$> mapM findExecutable
>           [ "alacritty"
>           , "termite"
>           , "xterm"
>           ]

>     xmproc <- spawnPipe $ xmproc hostname
>
#ifdef MIN_VERSION_dbus
>     mPulseClient <- connectPulseDBus
>     mPulsePath <- case mPulseClient of

We get the first path of the first sink which matches our pattern.

>       Just pc -> listToMaybe
>                   . (fmap fst)
>                   . filter (nameMatches "alsa_autput.*analog-stereo" . snd)
>                  <$> listSinks pc
>       Nothing -> return Nothing
>
>     let volumeHook = toList $ volume <$> mPulsePath <*> mPulseClient
>     let volumeKeys _ = concat . toList $ (\p c ->
>           [ ((0, xF86XK_AudioRaiseVolume), io (unmute p c >> modVolume    5000  p c) >> refresh)
>           , ((0, xF86XK_AudioLowerVolume), io (unmute p c >> modVolume (- 5000) p c) >> refresh)
>           , ((0, xF86XK_AudioMute)       , io (mute p c) >> refresh)
>           ]) <$> mPulsePath <*> mPulseClient
#else
>     let volumeHook = []
>     let volumeKeys _ = []
#endif /* MIN_VERSION_dbus */

Set up brightness stuff

>     mBacklight <- hasBacklight "intel_backlight"
>     let brightnessHook = maybe [] (pure . brightness) mBacklight

>     let brightnessKeys _ = fromMaybe [] $ mBacklight <&> \bl ->

        The refresh's here is to force a redraw of the status bar, otherwise
        my brightness indicator doesn't update when the keys are pressed.

>           [ ( (0, xF86XK_MonBrightnessDown),
>               io (updateBrightness bl $ -1000) >> refresh)
>           , ( (0, xF86XK_MonBrightnessUp),
>               io (updateBrightness bl  1000) >> refresh)
>           ]

>     let gray x  = dzenFg "#ABABAB" ++ x
>     let white x = dzenFg "white" ++ x
>     let fontWidth = 10
>     let w = 100 * length brightnessHook
>           + 100 * length volumeHook
>           + fontWidth * length " | 2021-09-28 05:22:08 (Tis v39) | BB 100% |  |  "
>     let loghookExtras = [ return . Just $ dzenMove RIGHT ++ dzenPos (- w)
>     -- "^ba(1920,_RIGHT)" --
>                         , date $ gray "%Y-%m-%d" ++ white " %T " ++ gray "(%a v%V)"
>                         , battery "BAT0"
>                         -- , swap
>                         ]
>                         ++ brightnessHook
>                         ++ volumeHook
>

Config Modifiers
================

- docks

- ewmh
Allows rofi to find windows


>     xmonad $ docks . ewmh $ def
>         { modMask = mod4Mask
>         , logHook = myLogHook xmproc loghookExtras
>         , clickJustFocuses  = False
>         , focusFollowsMouse = True
>         , keys = \conf -> M.fromList . mconcat . fmap ($ conf) $
>               [ otherKeys
>               , monitorKeys
>               , monitorFlipKeys nScreens
>               -- , movementKeys
>               , volumeKeys
>               , brightnessKeys
>               ]
>         , terminal = termCommand
>         , layoutHook = avoidStruts
>                      $ windowNavigation
>                      $ S.subTabbed
>                      $ B.boringWindows
>                      $ myLayouts
>         , manageHook
>                =  manageDocks
>               <+> scratchpadManageHook (W.RationalRect
>                                           0.25  -- left
>                                           0.25  -- right
>                                           0.5   -- width
>                                           0.5)  -- height
>               <+> myManageHook
>               <+> insertPosition Below Newer
>         , workspaces = ["term", "web"] ++ map show [3 .. nScreens]
>         , normalBorderColor  = "#1d1f21"
>         , focusedBorderColor = "#FF0000"
>         }


Appendix:
=========

In the key binding section this function is used.
It should be self explanitory enough (...). Here
it's used to lower the amount of boilerplate needed.

> bindWithAndWithoutShift noShift withShift binds
>   conf@XConfig {XMonad.modMask = modm} =
>   [ ((modm .|. m, k), f d)
>   | (k, d) <- binds
>   , (m, f) <- [ (0,         noShift)
>               , (shiftMask, withShift) ]]
