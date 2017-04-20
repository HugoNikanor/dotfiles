import XMonad
import Data.Monoid
import System.Exit

import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks

import XMonad.Hooks.DynamicLog --show workspace?
import XMonad.Util.Run --for spawnPipe & hPutStrLn

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Hooks.EwmhDesktops

import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig

import XMonad.Actions.SpawnOn

--------------------------------------------------------------------------------

import XMonad.Layout.Tabbed

myLayout = simpleTabbed

--myTerminal = "termite -c ~/.config/termite/desktop.conf"
myTerminal = "termite" :: String

-- myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False :: Bool

-- myClickJustFocuses :: Bool
myClickJustFocuses = False :: Bool

-- super key
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm,              xK_p     ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_c     ), kill) -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout) -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh) -- Resize viewed windows to the correct size
    , ((modm,               xK_Tab   ), windows W.focusDown) -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown) -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  ) -- Move focus to the master window
    , ((modm,               xK_Return), windows W.swapMaster) -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink) -- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand) -- Expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modm,               xK_v     ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
    , ((modm,               xK_z     ), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- Quit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
    , ((modm              , xK_s     ), spawn "systemctl suspend")
    , ((modm              , xK_b     ), spawn "firefox")
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((modm .|. mask, key), f sc)
       | (key, sc) <- zip [xK_a, xK_o, xK_e, xK_u] [0..]
       , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

-- End of myKeys

myManageHook :: ManageHook
myManageHook = composeAll
                [ className =? "MPlayer" --> doFloat
                , className =? "Gimp"    --> doFloat
                , className =? "Steam"   --> doFloat ]

-- myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "#1d1f21"
myFocusedBorderColor = "#de935f"

myBorderWidth = 1 :: Dimension


main = xmonad $
    ewmh defaultConfig
        { terminal          = myTerminal
        , focusFollowsMouse = myFocusFollowsMouse
        , modMask           = myModMask
        , keys              = myKeys
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor= myFocusedBorderColor
        , clickJustFocuses  = myClickJustFocuses
        -- , manageHook        = insertPosition Below Newer <+> myManageHook
        , manageHook        = manageDocks <+> manageHook defaultConfig <+> insertPosition Below Newer
        --, layoutHook        = avoidStruts $ layoutHook defaultConfig
        , layoutHook        = myLayout
        , handleEventHook   = handleEventHook defaultConfig <+> fullscreenEventHook
        , borderWidth       = myBorderWidth
        }

--main = xmonad $ ewmh defaultConfig{ handleEventHook =
--           handleEventHook defaultConfig <+> fullscreenEventHook }
