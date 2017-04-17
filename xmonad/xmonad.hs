import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

-- mod4mask?
import XMonad.Util.EZConfig

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout
import XMonad.Layout.Tabbed

import XMonad.Actions.PhysicalScreens

import Network.HostName

baseConfig = defaultConfig

-- xK_aring = å
-- xK_adiaeresis = ä
-- xK_odiaeresis = ö

-- This function should possibly be changed to take a context string instead of
-- just a hostname, this to simplify having multiple terminal types on one system
getTerminalCommand :: HostName -> String
getTerminalCommand hostName =
    case hostName of
        "arch2012" -> "termite -c ~/.config/termite/desktop.conf"
        "STATENS_laptop" -> "termite -c ~/.config/termite/laptop.conf"
        _ -> "termite -c ~/.config/termite/lysator.conf"


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    --[ ((modm, xK_Return), spawn $ getTerminalCommand "arch2012") -- TODO actually call getHostName
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf) -- TODO actually call getHostName
    , ((modm,               xK_y     ), spawn "dmenu_run")
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
    , ((modm, xK_w), windows W.focusDown) -- next window
    , ((modm, xK_m), windows W.focusUp)   -- prev window
    ]
    ++
    -- This is bindings for moving between physical screens
    -- TODO look into enabling the lower row to be primary, and the upper secondary
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_adiaeresis, xK_odiaeresis, xK_p, xK_o, xK_e, xK_u] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

myLayouts = simpleTabbed ||| Full

-- TODO thunderbird should be in a permanent workspace, which should be brought
-- up on demand instead of launching a new instance
--
-- Something Simmilar should probably be done for web browsers 
--
-- Emacs rolls on a proper client server setup

main = xmonad $ baseConfig {
      modMask           = mod4Mask
    , focusFollowsMouse = False
    , keys              = myKeys
    , terminal          = termCommand
    , layoutHook        = myLayouts
    }
    `additionalKeysP`
    -- TODO possibly simplyfy the "syntax" for these 'pre' bindings
    [ (pre ++ "f", spawn gBrowser)
    , (pre ++ "e", spawn gEmacs)
    , (pre ++ "t", spawn gMail)
    , (pre ++ "p", spawn gRun)
    , ("M-S-c", kill)
    , ("M-<Space>", sendMessage NextLayout) -- TODO get a better binding
    ] where pre      = "M-f "
            gBrowser = "firefox"
            gEmacs   = "emacsclient -c"
            gMail    = "thunderbird"
            gRun     = "dmenu_path | dmenu | $(which bash)"
            termCommand = getTerminalCommand "arch2012"

-- TODO mouse sholud move to currently active workspace when workspaces switch
