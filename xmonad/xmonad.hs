{-# OPTIONS_GHC -W #-}

import System.IO (hPutStrLn)
import Network.HostName (HostName, getHostName)

import XMonad

import XMonad.Actions.PhysicalScreens (viewScreen, PhysicalScreen (..), sendToScreen)
import XMonad.Actions.DynamicWorkspaces (selectWorkspace, removeWorkspace, removeEmptyWorkspace, addWorkspacePrompt, withWorkspace, renameWorkspace)
import XMonad.Actions.CycleWS (prevScreen, nextScreen, toggleWS)
import XMonad.Actions.Warp (banish, Corner (..))
import XMonad.Actions.Navigation2D (windowGo, windowSwap)

import XMonad.Hooks.InsertPosition (Position (Below), Focus (Newer), insertPosition)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Dishes (Dishes (Dishes))

import XMonad.Prompt

import XMonad.Util.Types (Direction2D (U, D, L, R))
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
* Possibly add scratchpad (floating term) (I have that for gvim now!)
* Look through which of the imports are needed
* If only one program is i a workspace the workspace should change name to that
-}

myFont = "Iosevka Slab-11"

-- This function should possibly be changed to take a context string instead of
-- just a hostname, this to simplify having multiple terminal types on one system
-- `hostname -y` return "lysator" on some systems, "(none)" on other
getTerminalCommand :: HostName -> String
getTerminalCommand "arch2012"       = "termite -c ~/.config/termite/desktop.conf"
getTerminalCommand "STATENS_laptop" = "termite -c ~/.config/termite/laptop.conf"
getTerminalCommand _                = "/home/hugo/bin/termite -c ~/.config/termite/lysator.conf"

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
    -- This doesn't work in the easy config, for some reason
    , ((modm, xK_t), withFocused $ windows . W.sink)
    ]

{-
notSoSimlpeBindings :: [((KeyMask, KeySym), X ())]
notSoSimlpeBindings =
        [ ((modMask              , xK_odiaeresis), (viewScreen $ P 3) >> banish LowerRight)
        , ((modMask .|. shiftMask, xK_odiaeresis), sendToScreen $ P 3)
        ]
        -}
xmobarCurrentWorkspaceColor = "#FFB6B0"
xmobarTitleColor = "#CEFFAC"

-- "Steam - News (1 of 2)"
myManageHook = composeAll
    [ className =? "Gvim" --> doFloat
    , className =? "Pinentry" --> doFloat
    , className =? "Floating" --> doFloat
    , className =? "Gimp" --> doShift "gimp"
    , className =? "Steam" --> doShift "steam"
    , isDialog --> doFloat

    -- Who doesn't this work!
    -- , className =? "Gimp" <&&> (appName =? "Toolbox" <||> title =? "Toolbox") --> doFloat ]
    --, className =? "Gimp" --> doFloat ]
    , title =? "Toolbox" --> doFloat ]

main = do
    termCommand <- getTerminalCommand <$> getHostName
    -- nScreens    <- countScreens
    -- xmproc      <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
    xmonad $ def { modMask            = mod4Mask
                 , focusFollowsMouse  = False
                 , clickJustFocuses   = False
                 , keys               = myKeys
                 , terminal           = termCommand
                 --, layoutHook         = subTabbed $ B.boringWindows $ ifWider tallThreshold wideLayouts tallLayouts
                 , layoutHook         = ifWider tallThreshold wideLayouts tallLayouts
                 , manageHook         = myManageHook <+> insertPosition Below Newer
                 , workspaces         = ["term", "web"]
                 , normalBorderColor  = "#1d1f21"
                 , focusedBorderColor = "#FF0000"
                 } `additionalKeysP`
                 [ (pre 'b', spawn gBrowser)
                 , (pre 'e', spawn gEmacs)
                 , (pre 'y', spawn gRun)
                 , (pre 'p', spawn gPass)
                 , (pre 'i', spawn gIrc)
                 , (pre 's', spawn gSound)
                 , (pre 'f', spawn gFiles)
                 , (pre 'n', spawn gQuickNote)

                 -- , ("M-t", withFocused $ windows . W.sink)

                 , ("M-S-<Return>", windows W.swapMaster)

                 --, ("M-j", B.focusDown)
                 --, ("M-k", B.focusUp)
                 --, ("M-S-j", windows W.swapDown)
                 --, ("M-S-k", windows W.swapUp)

                 , ("M-m", sendMessage Shrink)
                 , ("M-w", sendMessage Expand)

                 , ("M-S-m", sendMessage $ IncMasterN    1)
                 , ("M-S-w", sendMessage $ IncMasterN $ -1)

                 , ("M-o"  , (viewScreen $ P 0) >> banish LowerRight)
                 , ("M-S-o", (sendToScreen $ P 0))
                 , ("M-e"  , (viewScreen $ P 1) >> banish LowerRight)
                 , ("M-S-e", (sendToScreen $ P 1))
                 -- , ("M-u"  , (viewScreen $ P 2) >> banish LowerRight)
                 -- , ("M-S-u", (sendToScreen $ P 2))
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

                 -- , ("M-x", sendMessage $ ToggleStruts)


                 , ("M-s", toggleWS)

                 -- Do I even want these?
                 -- especcially if they don't work on a per-screen basis
                 -- Possibly write some own which works together with IndependentScreens
                 , ("M-g"  , prevScreen)
                 , ("M-c"  , nextScreen)
                 --, ("M-S-g", shiftPrevScreen)
                 --, ("M-S-c", shiftNextScreen)

                 -- , ("M-C-k", withFocused $ sendMessage . mergeDir W.focusUp')
                 -- , ("M-C-j", withFocused $ sendMessage . mergeDir W.focusDown')
                 -- , ("M-C-z", withFocused $ sendMessage . MergeAll)
                 -- , ("M-C-v", withFocused $ sendMessage . UnMerge)

                 --, ("M-m", onGroup W.focusUp')
                 --, ("M-w", onGroup W.focusDown')

                 , ("M-y", spawn gRun)

                 , ("M-q", spawn xmonadRe)
                 , ("M-t", withFocused $ windows . W.sink)

                 , ("M-a", sendMessage $ IncMasterN 1)
                 , ("M-.", sendMessage $ IncMasterN (- 1))

                 , ("M-S-c", kill)
                 , ("M-n", sendMessage NextLayout)

                 , ("M-<Space> M-<Space>", selectWorkspace myXPConfig)
                 , ("M-<Space> <Space>", selectWorkspace myXPConfig)

                 , ("M-<Space> d", removeWorkspace)
                 , ("M-<Space> M-d", removeEmptyWorkspace)
                 , ("M-<Space> a", addWorkspacePrompt myXPConfig)
                 , ("M-<Space> s", withWorkspace myXPConfig (windows . W.shift))
                 -- This should also select that workspace
                 , ("M-<Space> S-s", withWorkspace myXPConfig (windows . W.shift))

                 , ("M-<Space> r", renameWorkspace myXPConfig { autoComplete = Nothing })

                 ] where pre = \a -> "M-f " ++ [a]
                         gBrowser   = "google-chrome"
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

                         tallThreshold = 1200
                         wideLayouts = Tall 1 (3/100) (3/5) ||| GridRatio (4/3)
                         tallLayouts = Dishes 1 (1/4) ||| GridRatio (4/3)


