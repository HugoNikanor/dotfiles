import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

import XMonad.Util.Types

-- mod4mask?
import XMonad.Util.EZConfig

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout
import XMonad.Layout.Tabbed

import XMonad.Actions.PhysicalScreens

import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.BoringWindows as B

import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.IndependentScreens

import XMonad.Actions.CycleRecentWS

import XMonad.Hooks.InsertPosition

import Network.HostName

-- xK_aring = å
-- xK_adiaeresis = ä
-- xK_odiaeresis = ö

-- This function should possibly be changed to take a context string instead of
-- just a hostname, this to simplify having multiple terminal types on one system
getTerminalCommand :: HostName -> String
getTerminalCommand hostName =
    case hostName of
        "arch2012"       -> "termite -c ~/.config/termite/desktop.conf"
        "STATENS_laptop" -> "termite -c ~/.config/termite/laptop.conf"
        _                -> "termite -c ~/.config/termite/lysator.conf"


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_y     ), spawn "dmenu_run")
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- This is bindings for moving between physical screens
    -- TODO look into enabling the lower row to be primary, and the upper secondary
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_adiaeresis, xK_odiaeresis, xK_p, xK_o, xK_e, xK_u] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

--myLayouts = subTabbed $ B.boringWindows $ simpleTabbed ||| Full ||| Tall 1 (3/100) (1/2)
myLayouts = subTabbed $ B.boringWindows $ Tall 1 (3/100) (3/5) ||| Full

-- TODO thunderbird should be in a permanent workspace, which should be brought
-- up on demand instead of launching a new instance
--
-- Something Simmilar should probably be done for web browsers
--
-- Emacs rolls on a proper client server setup

main = do
    termCommand <- getTerminalCommand <$> getHostName
    xmonad $ def {
          modMask           = mod4Mask
        , focusFollowsMouse = False
        , clickJustFocuses  = False
        , keys              = myKeys
        , terminal          = termCommand
        , layoutHook        = myLayouts
        , manageHook        = insertPosition Below Newer
        }
        `additionalKeysP`
        [ (pre ++ "b", spawn gBrowser)
        , (pre ++ "e", spawn gEmacs)
        , (pre ++ "m", spawn gMail)
        , (pre ++ "y", spawn gRun)
        , (pre ++ "p", spawn gPass)
        , (pre ++ "i", spawn gIrc)
        , (pre ++ "s", spawn gSound)

        , ("M-j", B.focusDown)
        , ("M-k", B.focusUp)
        , ("M-S-j", windows W.swapDown)
        , ("M-S-k", windows W.swapUp)

        , ("M-S-h", sendMessage Shrink)
        , ("M-S-l", sendMessage Expand)

        , ("M-h", sendMessage $ IncMasterN    1)
        , ("M-l", sendMessage $ IncMasterN $ -1)


        , ("M-C-k", withFocused $ sendMessage . mergeDir W.focusUp')
        , ("M-C-j", withFocused $ sendMessage . mergeDir W.focusDown')
        , ("M-C-z", withFocused $ sendMessage . MergeAll)
        , ("M-C-v", withFocused $ sendMessage . UnMerge)

        , ("M-m", onGroup W.focusUp')
        , ("M-w", onGroup W.focusDown')

        , ("M-S-c", kill)
        , ("M-<Space>", sendMessage NextLayout) -- TODO get a better binding

        , ("M-<Tab>", cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
        ] where pre         = "M-f "
                gBrowser    = "firefox"
                gEmacs      = "emacsclient -c"
                gMail       = "thunderbird"
                gRun        = "dmenu_path | dmenu | $(which bash)"
                gPass       = "passmenu"
                gIrc        = "xterm -e ssh irc screen -x"
                gSound      = "pavucontrol"

    -- TODO mouse sholud move to currently active workspace when workspaces switch
