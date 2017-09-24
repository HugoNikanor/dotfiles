import System.IO (hPutStrLn)
import Network.HostName (HostName, getHostName)

import XMonad

import XMonad.Actions.PhysicalScreens (viewScreen, PhysicalScreen (..), sendToScreen)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS (prevScreen, nextScreen, toggleWS)
import XMonad.Actions.Warp (banish, Corner (..))
import XMonad.Actions.WindowGo
import XMonad.Actions.Navigation2D

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks --(manageDocks, ToggleStruts)
import XMonad.Hooks.InsertPosition

import XMonad.Layout
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.PerScreen
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
--import XMonad.Layout.Roledex
import XMonad.Layout.Dishes (Dishes (..))
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Maximize (maximize)

import XMonad.Prompt

import XMonad.Util.SpawnOnce
import XMonad.Util.Types
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.BoringWindows as B

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

{-
notSoSimlpeBindings :: [((KeyMask, KeySym), X ())]
notSoSimlpeBindings =
        [ ((modMask              , xK_odiaeresis), (viewScreen $ P 3) >> banish LowerRight)
        , ((modMask .|. shiftMask, xK_odiaeresis), sendToScreen $ P 3)
        ]
        -}

list :: a -> [a]
list a = [a]

xmobarCurrentWorkspaceColor = "#FFB6B0"
xmobarTitleColor = "#CEFFAC"

-- composeOne vs composeAll
myManageHook = composeAll
    [ className =? "Gvim" --> doFloat ]

main = do
    termCommand <- getTerminalCommand <$> getHostName
    nScreens    <- countScreens
    xmproc      <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ def { modMask            = mod4Mask
                 , focusFollowsMouse  = False
                 , clickJustFocuses   = False
                 , keys               = myKeys
                 , terminal           = termCommand
                 --, layoutHook         = subTabbed $ B.boringWindows $ ifWider tallThreshold wideLayouts tallLayouts
                 , layoutHook         = B.boringWindows $ maximize $ ifWider tallThreshold wideLayouts tallLayouts
                 , manageHook         = manageDocks <+> myManageHook <+> insertPosition Below Newer
                 , workspaces         = ["web", "irc", "mail", "term"]
                 , normalBorderColor  = "#1d1f21"
                 , focusedBorderColor = "#FF0000"
                 , logHook            = dynamicLogWithPP $ xmobarPP
                        { ppOutput  = hPutStrLn xmproc
                        , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 100
                        , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                        , ppSep     = "    "
                        }
                 } `additionalKeysP`
                 [ (pre 'b', spawn gBrowser)
                 , (pre 'e', spawn gEmacs)
                 , (pre 'y', spawn gRun)
                 , (pre 'p', spawn gPass)
                 , (pre 'i', spawn gIrc)
                 , (pre 's', spawn gSound)
                 , (pre 'f', spawn gFiles)
                 , (pre 'n', spawn gQuickNote)

                 , ("M-t", withFocused $ windows . W.sink)

                 --, ("M-j", B.focusDown)
                 --, ("M-k", B.focusUp)
                 --, ("M-S-j", windows W.swapDown)
                 --, ("M-S-k", windows W.swapUp)

                 --, ("M-S-h", sendMessage Shrink)
                 --, ("M-S-l", sendMessage Expand)

                 --, ("M-h", sendMessage $ IncMasterN    1)
                 --, ("M-l", sendMessage $ IncMasterN $ -1)

                 , ("M-o"  , (viewScreen $ P 0) >> banish LowerRight)
                 , ("M-S-o", (sendToScreen $ P 0))
                 , ("M-e"  , (viewScreen $ P 1) >> banish LowerRight)
                 , ("M-S-e", (sendToScreen $ P 1))
                 , ("M-u"  , (viewScreen $ P 2) >> banish LowerRight)
                 , ("M-S-u", (sendToScreen $ P 2))
                 -- , ("M-<oadiaeresis>", (viewScreen $ P 2) >> banish LowerRight)
                 -- , ("M-S-<oadiaeresis>", (sendToScreen $ P 2))

                 -- Can keybinds be dependent on current layout?
                 , ("M-l", windowGo R False >> banish LowerRight)
                 , ("M-h", windowGo L False >> banish LowerRight)
                 , ("M-k", windowGo U False >> banish LowerRight)
                 , ("M-j", windowGo D False >> banish LowerRight)

                 , ("M-S-l", windowSwap R False)
                 , ("M-S-h", windowSwap L False)
                 , ("M-S-k", windowSwap U False)
                 , ("M-S-j", windowSwap D False)

                 , ("M-x", sendMessage $ ToggleStruts)


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

                 , ("M-<Space> r", renameWorkspace myXPConfig { autoComplete = Nothing })

                 --, ("M-u", selectWorkspace def)
                 --, ("M-i", spawnToWorkspace "mail" "xterm")
                 ] where pre = ("M-f " ++) . list
                         gBrowser   = "firefox"
                         gEmacs     = "emacsclient -c"
                         gMail      = "thunderbird"
                         gRun       = "dmenu_path | dmenu | $(which bash)"
                         gPass      = "passmenu"
                         gIrc       = "xterm -e ssh irc -t screen -x"
                         gSound     = "pavucontrol"
                         gFiles     = "thunar"
                         gQuickNote = "gvim +'setlocal buftype=nofile' -n" --s <(echo -n i)"

                         xmonadRe = "xmonad --recompile; xmonad --restart"

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


