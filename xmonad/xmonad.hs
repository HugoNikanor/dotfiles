import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

import XMonad.Util.Types

-- mod4mask?
import XMonad.Util.EZConfig

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout

import XMonad.Actions.PhysicalScreens

import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.BoringWindows as B

import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.IndependentScreens
import XMonad.Actions.CycleWS

import XMonad.Hooks.InsertPosition
import Network.HostName

-- xK_aring = å
-- xK_adiaeresis = ä
-- xK_odiaeresis = ö

{-
TODO
====
* Make it so that when a workspace is pulled from a monitor spawn a new
  workspace there, this is for better compability with how thunderbird
  currently works
* Auto launch thunderbird in mail workspace
* Possibly add scratchpad (floating term)
* Set up for some floating windows
* Add key to tile a floating window
* Allow transparent windows
* Look through which of the imports are needed
-}

-- This function should possibly be changed to take a context string instead of
-- just a hostname, this to simplify having multiple terminal types on one system
-- `hostname -y` return "lysator" on some systems, "(none)" on other
getTerminalCommand :: HostName -> String
getTerminalCommand hostName =
    case hostName of
        "arch2012"       -> "termite -c ~/.config/termite/desktop.conf"
        "STATENS_laptop" -> "termite -c ~/.config/termite/laptop.conf"
        _                -> "termite -c ~/.config/termite/lysator.conf"

-- TODO figure out how to run this from main
-- TODO and check if a window can be spawned without viewing that window
spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace workspace program = do
    spawn program
    windows $ W.greedyView workspace


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_y     ), spawn "dmenu_run")
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
    ]
    ++
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- This is bindings for moving between physical screens
    -- TODO look into enabling the lower row to be primary, and the upper secondary
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_adiaeresis, xK_odiaeresis, xK_p, xK_o, xK_e, xK_u] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]


main = do
    termCommand <- getTerminalCommand <$> getHostName
    nScreens    <- countScreens
    --addWorkspace "mail"
    --withWorkspace def $ sendMessage . spawn "thunderbird"
    xmonad $ def {
          modMask           = mod4Mask
        , focusFollowsMouse = False
        , clickJustFocuses  = False
        , keys              = myKeys
        , terminal          = termCommand
        , layoutHook        = subTabbed $ B.boringWindows $ Tall 1 (3/100) (3/5) ||| Full
        , manageHook        = insertPosition Below Newer
        , workspaces        = (withScreens nScreens $ show <$> [1..9]) ++ ["mail"]
        }
        `additionalKeysP`
        [ (pre ++ "b", spawn gBrowser)
        , (pre ++ "e", spawn gEmacs)
        --, (pre ++ "m", spawn gMail)
        , (pre ++ "y", spawn gRun)
        , (pre ++ "p", spawn gPass)
        , (pre ++ "i", spawn gIrc)
        , (pre ++ "s", spawn gSound)
        , (pre ++ "f", spawn gFiles)

        , ("M-b", toggleOrView "mail")

        , ("M-j", B.focusDown)
        , ("M-k", B.focusUp)
        , ("M-S-j", windows W.swapDown)
        , ("M-S-k", windows W.swapUp)

        , ("M-S-h", sendMessage Shrink)
        , ("M-S-l", sendMessage Expand)

        , ("M-h", sendMessage $ IncMasterN    1)
        , ("M-l", sendMessage $ IncMasterN $ -1)

        , ("M-s", toggleWS)

        -- Do I even want these?
        -- especcially if they don't work on a per-screen basis
        , ("M-g"  , prevScreen)
        , ("M-c"  , nextScreen)
        , ("M-S-g", shiftPrevScreen)
        , ("M-S-c", shiftNextScreen)

        , ("M-C-k", withFocused $ sendMessage . mergeDir W.focusUp')
        , ("M-C-j", withFocused $ sendMessage . mergeDir W.focusDown')
        , ("M-C-z", withFocused $ sendMessage . MergeAll)
        , ("M-C-v", withFocused $ sendMessage . UnMerge)

        , ("M-m", onGroup W.focusUp')
        , ("M-w", onGroup W.focusDown')

        , ("M-S-c", kill)
        , ("M-<Space>", sendMessage NextLayout) -- TODO get a better binding

        --, ("M-u", selectWorkspace def)
        --, ("M-i", spawnToWorkspace "mail" "xterm")
        ] where pre         = "M-f "
                gBrowser    = "firefox"
                gEmacs      = "emacsclient -c"
                gMail       = "thunderbird"
                gRun        = "dmenu_path | dmenu | $(which bash)"
                gPass       = "passmenu"
                gIrc        = "xterm -e ssh irc screen -x"
                gSound      = "pavucontrol"
                gFiles      = "thunar"

    -- TODO mouse sholud move to currently active workspace when workspaces switch
