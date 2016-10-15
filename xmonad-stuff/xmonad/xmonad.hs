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

--import XMonad.Hooks.ServerMode
--import XMonad.Actions.Commands
import XMonad.Hooks.EwmhDesktops

import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig

import XMonad.Actions.SpawnOn

--myTerminal = "termite"
myTerminal = "start-termite"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_l     ), spawn "start-termite large")

    -- launch dmenu
    --, ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm,              xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    --, ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm .|. shiftMask, xK_c     ), spawn "texit")

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm,               xK_v     ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm,               xK_z     ), sendMessage (IncMasterN (-1))) 

    , ((modm,               xK_w     ), spawn "dmenu_info_bar")
    

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    --, ((modm              , xK_d     ), spawnOn "workspace1" "emacs")
    , ((modm              , xK_d     ), spawn "notify-send $(date +%s)")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm              , xK_s     ), spawn "systemctl suspend")

    , ((modm              , xK_b     ), spawn "google-chrome-stable --disable-gpu")

    --,((modm , xF86XK_MonBrightnessDown ), spawn "xbacklight -10" )
    ,((modm               , xK_Next  ), spawn "xbacklight -10" )
    ,((modm               , xK_Prior  ), spawn "xbacklight +10" )

    ,((noModMask          , xK_Scroll_Lock ), spawn "date +'Time: %R' | dmenu  -nb \"#1d1f21\" -nf \"#c8c8c6\"" )


    ]
    ++

    --[((xF86XK_MonBrightnessUp), spawn "xbacklight +10")]

    -- ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
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

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "#1d1f21"
myFocusedBorderColor = "#de935f"

myBorderWidth :: Dimension
myBorderWidth = 1


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
        , layoutHook        = avoidStruts $ layoutHook defaultConfig
        , handleEventHook   = handleEventHook defaultConfig <+> fullscreenEventHook
        , borderWidth       = myBorderWidth
        }

--main = xmonad $ ewmh defaultConfig{ handleEventHook =
--           handleEventHook defaultConfig <+> fullscreenEventHook }
