import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

import XMonad.Util.Types

import XMonad.Prompt

-- mod4mask?
import XMonad.Util.EZConfig

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.PerScreen

import XMonad.Actions.PhysicalScreens

import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.BoringWindows as B

import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.IndependentScreens
import XMonad.Actions.CycleWS

import XMonad.Actions.Warp
import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D
import XMonad.Layout.Maximize

import XMonad.Layout.Roledex
import XMonad.Layout.Dishes
--import XMonad.Layout.Simplest

import XMonad.Hooks.InsertPosition
import Network.HostName

-- xK_aring      = å
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
* Mouse sholud move to currently active workspace when workspaces switch
-}

myFont = "Iosevka Slab-11"

-- This function should possibly be changed to take a context string instead of
-- just a hostname, this to simplify having multiple terminal types on one system
-- `hostname -y` return "lysator" on some systems, "(none)" on other
getTerminalCommand :: HostName -> String
getTerminalCommand "arch2012"       = "termite -c ~/.config/termite/desktop.conf"
getTerminalCommand "STATENS_laptop" = "termite -c ~/.config/termite/laptop.conf"
getTerminalCommand _                = "termite -c ~/.config/termite/lysator.conf"

-- TODO figure out how to run this from main
-- TODO and check if a window can be spawned without viewing that window
spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace workspace program = do
    spawn program
    windows $ W.greedyView workspace

{-
bootstrap :: X()
bootstrap = do
    spawnToWorkspace "firefox" "web"
    spawnToWorkspace "term"
-}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_Tab ), windows W.focusDown)
    ]

main = do
    termCommand <- getTerminalCommand <$> getHostName
    nScreens    <- countScreens
    --addWorkspace "mail"
    --withWorkspace def $ sendMessage . spawn "thunderbird"
    xmonad $ def {
          modMask            = mod4Mask
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , keys               = myKeys
        , terminal           = termCommand
        --, layoutHook         = subTabbed $ B.boringWindows $ ifWider tallThreshold wideLayouts tallLayouts
        , layoutHook         = B.boringWindows $ maximize $ ifWider tallThreshold wideLayouts tallLayouts
        , manageHook         = insertPosition Below Newer
        -- , workspaces         = (withScreens nScreens $ show <$> [1..9]) ++ [mailWS]
        , workspaces         = ["web", "irc", "mail", "term"]
        , normalBorderColor  = "#1d1f21"
        , focusedBorderColor = "#FF0000"
        }
        `additionalKeysP`
        [ (pre "b", spawn gBrowser)
        , (pre "e", spawn gEmacs)
        --, (pre ++ "m", spawn gMail)
        , (pre "y", spawn gRun)
        , (pre "p", spawn gPass)
        , (pre "i", spawn gIrc)
        , (pre "s", spawn gSound)
        , (pre "f", spawn gFiles)

        --, ("M-b", toggleOrView mailWS)

        , ("M-t", withFocused $ windows . W.sink)

        --, ("M-j", B.focusDown)
        --, ("M-k", B.focusUp)
        --, ("M-S-j", windows W.swapDown)
        --, ("M-S-k", windows W.swapUp)

        --, ("M-S-h", sendMessage Shrink)
        --, ("M-S-l", sendMessage Expand)

        --, ("M-h", sendMessage $ IncMasterN    1)
        --, ("M-l", sendMessage $ IncMasterN $ -1)

        , ("M-o", (viewScreen $ P 0) >> banish LowerRight)
        , ("M-e", (viewScreen $ P 1) >> banish LowerRight)
        , ("M-S-o", (sendToScreen $ P 0))
        , ("M-S-e", (sendToScreen $ P 1))

        -- Can keybinds be dependent on current layout?
        , ("M-l", windowGo R False >> banish LowerRight)
        , ("M-h", windowGo L False >> banish LowerRight)
        , ("M-k", windowGo U False >> banish LowerRight)
        , ("M-j", windowGo D False >> banish LowerRight)

        , ("M-S-l", windowSwap R False)
        , ("M-S-h", windowSwap L False)
        , ("M-S-k", windowSwap U False)
        , ("M-S-j", windowSwap D False)


        , ("M-s", toggleWS)

        -- Do I even want these?
        -- especcially if they don't work on a per-screen basis
        -- Possibly write some own which works together with IndependentScreens
        , ("M-g"  , prevScreen)
        , ("M-c"  , nextScreen)
        --, ("M-S-g", shiftPrevScreen)
        --, ("M-S-c", shiftNextScreen)

        , ("M-C-k", withFocused $ sendMessage . mergeDir W.focusUp')
        , ("M-C-j", withFocused $ sendMessage . mergeDir W.focusDown')
        , ("M-C-z", withFocused $ sendMessage . MergeAll)
        , ("M-C-v", withFocused $ sendMessage . UnMerge)

        --, ("M-m", onGroup W.focusUp')
        --, ("M-w", onGroup W.focusDown')

        , ("M-y", spawn gRun)

        , ("M-q", spawn xmonadRe)
        , ("M-t", withFocused $ windows . W.sink)

        , ("M-a", sendMessage $ IncMasterN 1)
        , ("M-.", sendMessage $ IncMasterN (- 1))

        , ("M-S-c", kill)
        , ("M-m", sendMessage NextLayout) -- TODO get a better binding

        , ("M-<Space> M-<Space>", selectWorkspace myXPConfig)
        , ("M-<Space> <Space>", selectWorkspace myXPConfig)

        , ("M-<Space> d", removeWorkspace)
        , ("M-<Space> M-d", removeEmptyWorkspace)
        , ("M-<Space> a", addWorkspacePrompt myXPConfig)
        , ("M-<Space> s", withWorkspace myXPConfig (windows . W.shift))
        -- This should also select that workspace
        , ("M-<Space> S-s", withWorkspace myXPConfig (windows . W.shift))

        --, ("M-u", selectWorkspace def)
        --, ("M-i", spawnToWorkspace "mail" "xterm")
        ] where pre      = ("M-f " ++)

                gBrowser = "firefox"
                gEmacs   = "emacsclient -c"
                gMail    = "thunderbird"
                gRun     = "dmenu_path | dmenu | $(which bash)"
                gPass    = "passmenu"
                gIrc     = "xterm -e ssh irc -t screen -x"
                gSound   = "pavucontrol"
                gFiles   = "thunar"

                xmonadRe = "xmonad --recompile; xmonad --restart"
                mailWS   = "mail"

                myXPConfig = defaultXPConfig
                    { position = Top -- CenteredAt
                    , historySize = 1000
                    , autoComplete = Just 1
                    , font = myFont
                    -- , searchPredicate = isInfixOf
                    }

                -- TODO possibly auto get this number dependend on the system
                -- possibly also change to a check if a screen is higher than
                -- it is wide, and then change layouts
                tallThreshold = 1200
                --wideLayouts   = Tall 1 (3/100) (3/5) ||| Full
                wideLayouts = GridRatio (4/3) ||| Full
                --tallLayouts = Grid ||| Full
                tallLayouts = {- Roledex ||| -} Dishes 1 (1/4) ||| Full
                --tallLayouts = subLayout [0, 1] $  $ Dishes 2 (1/6)
                --tallLayouts = {- addTabs shrinkText -} subLayout [0, 1, 2, 3] (Simplest ||| Tall 1 0.2 0.5 ||| Circle) $ Tall 1 0.2 0.5 ||| Full
                --tallLayouts = {- addTabs shrinkText -} subLayout [2] (Circle) $ Tall 1 0.2 0.5 ||| Full
                ---tallLayouts = {- addTabs shrinkText -} subLayout [0,1]  (Roledex) $ Dishes 2 (1/6)  ||| Full
                --
                --tallLayouts = subLayout [0, 1] (Full ||| Grid )

