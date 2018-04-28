> module Config where
>
> import System.IO (hPutStrLn)
> import Network.HostName (HostName, getHostName)
> import Data.List (isInfixOf)
> 
> import XMonad
> 
> import XMonad.Actions.PhysicalScreens
>     (viewScreen, PhysicalScreen (..), sendToScreen)
> import XMonad.Actions.DynamicWorkspaces
>     ( selectWorkspace
>     , removeWorkspace
>     , removeEmptyWorkspace
>     , addWorkspacePrompt
>     , withWorkspace
>     , renameWorkspace )
> import XMonad.Actions.CycleWS (prevScreen, nextScreen, toggleWS)
> import XMonad.Actions.Warp (banish, Corner (..))
> import XMonad.Actions.Navigation2D (windowGo, windowSwap)
> 
> import XMonad.Hooks.InsertPosition
>     (Position (Below), Focus (Newer), insertPosition)
> import XMonad.Hooks.ManageHelpers (isDialog)
> 
> import XMonad.Layout.Grid (Grid (..))
> import XMonad.Layout.PerScreen (ifWider)
> import XMonad.Layout.Dishes (Dishes (Dishes))
> import XMonad.Layout.Decoration (shrinkText)
> import XMonad.Layout.DwmStyle (dwmStyle)
> import XMonad.Layout.OneBig (OneBig (OneBig))
> 
> import XMonad.Prompt (XPConfig (..), defaultXPConfig, XPPosition (Top))
> import XMonad.Prompt.Input (inputPrompt)
> import XMonad.Prompt.Shell (shellPrompt)
> import XMonad.Prompt.XMonad (xmonadPrompt)
> 
> import XMonad.Util.Types (Direction2D (U, D, L, R))
> import XMonad.Util.EZConfig
> 
> import qualified Data.Map as M
> import qualified XMonad.StackSet as W
> import qualified XMonad.Layout.BoringWindows as B

The xK names of the swedish letters

xK_aring      = å
xK_adiaeresis = ä
xK_odiaeresis = ö

TODO
====
* Possibly add scratchpad (floating term) (I have that for gvim now!)
* If only one program is in a workspace the workspace should change name to that

> myFont = "Iosevka Slab-11"

This function should possibly be changed to take a context string instead of
just a hostname, this to simplify having multiple terminal types on one system
`hostname -y` return "lysator" on some systems, "(none)" on other

> getTerminalCommand :: HostName -> String
> getTerminalCommand "arch2012"       = "termite -c ~/.config/termite/desktop.conf"
> getTerminalCommand "STATENS_laptop" = "termite -c ~/.config/termite/laptop.conf"
> getTerminalCommand _                = "/home/hugo/bin/termite -c ~/.config/termite/lysator.conf"

TODO figure out how to run this from main
TODO and check if a window can be spawned without viewing that window

> spawnToWorkspace :: String -> String -> X ()
> spawnToWorkspace workspace program = do
>     spawn program
>     windows $ W.greedyView workspace

------------------------------------------------------------

Keys are currently bound in two sepparate places. Both here,
where I have complete controll over Haskell, and below where
I allow EZConfig to do the work. The settings that are set
here is either due to them requiring some data sent to them
in unusual ways (such as =XMonad.terminal=), or depend on
actuall code (see monitor movement bindings).

----

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

footnote:
Choose one of these, depending on the current monitor setup.
(`x' declared as binding for /no monitor/)

> x = -1 :: Int

> -- (a, b, c) = (_, 0, _)
> -- (d, e, f) = (_, 1, 2)

> (a, b, c) = (x, 0, x)
> (d, e, f) = (1, 2, 3)

> monitorKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
> monitorKeys conf@(XConfig {XMonad.modMask = modm}) = 
>   [ ((modm .|. m, k), f i)
>   | (k, i) <- [ (xK_adiaeresis, a)
>               , (xK_odiaeresis, b)
>               , (xK_p, c)
>               , (xK_o, d)
>               , (xK_e, e)
>               , (xK_u, f) ]
>   , (m, f) <- zip [0, shiftMask]
>               [ \i -> (viewScreen $ P i) >> banish LowerRight
>               , \i -> (sendToScreen $ P i) ]
>   ]

----

> otherKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
> otherKeys conf@(XConfig {XMonad.modMask = modm}) = 
>     [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
>     , ((modm, xK_Tab ), windows W.focusDown)
>     , ((modm .|. shiftMask, xK_Tab ), windows W.focusUp)
>     -- This doesn't work in the easy config, for some reason
>     , ((modm, xK_t), withFocused $ windows . W.sink)
>     ]

Finnaly we put the two config parts together.
NOTE I'm note sure what the =conf@...= part actually does,
but I believe that it sends =conf= as a parameter, and break
out the modMask attribute into the parameter =modm=.

> myKeys conf@(XConfig {XMonad.modMask = modm}) =
>   M.fromList $ otherKeys   conf
>             ++ monitorKeys conf

------------------------------------------------------------

"Steam - News (1 of 2)"

> myManageHook = composeAll
>     [ className =? "Gvim" --> doFloat
>     , className =? "Pinentry" --> doFloat
>     , className =? "Floating" --> doFloat
>     , className =? "Gimp" --> doShift "gimp"
>     , className =? "Steam" --> doShift "steam"
>     , className =? "Xmessage" --> doFloat
>     , isDialog --> doFloat
> 
>     -- Who doesn't this work!
>     -- , className =? "Gimp" <&&> (appName =? "Toolbox" <||> title =? "Toolbox") --> doFloat ]
>     --, className =? "Gimp" --> doFloat ]
>     , title =? "Toolbox" --> doFloat ]
> 
> shiftAndGo :: Maybe String -> X ()
> shiftAndGo Nothing = return ()
> shiftAndGo (Just str) =
>     windows $ do
>         W.shift $ str
>         W.greedyView $ str

This is the main part of the config. It's currently all
grouped together, this might change in the future, if I
can be bothered to do it.

The main reason for breaking it upp is that it will
allow resources belonging to parts be close to them,
instead of the current set-up where they are quite far
sepparated.

> main = do
>     termCommand <- getTerminalCommand <$> getHostName
>     -- nScreens    <- countScreens
>     -- xmproc      <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
>     xmonad $ def {

We use super for xmonad actions, since it shouldn't be used by any
other program.

> modMask = mod4Mask

A mouse click in a window should always count as if I actually press
there. Focus also doesn't follow the mouse, allowing me to scroll and
type in different windows.

This makes it really hard to focus a window without sending an action
to it, but that's hardly ever needed.

> , clickJustFocuses  = False
> , focusFollowsMouse = False

TODO gather all key binds in one place
TODO also document terminal commands

> , keys     = myKeys
> , terminal = termCommand

A different set of layouts should be used for screens that are tall
and wide. Decorations are the small labels on each window telling its
name.

> , layoutHook = decoration $ ifWider tallThreshold wideLayouts tallLayouts

> , manageHook = myManageHook <+> insertPosition Below Newer

Default workspace names, there should be at least as many items in
this list as there are physical monitors.

> , workspaces = ["term", "web"]

Borders are red and clearly visible, unfocused windows have the same
gray color as I try to have everything on my system.

> , normalBorderColor  = "#1d1f21"
> , focusedBorderColor = "#FF0000"


>                  } `additionalKeysP`
>                  [ (pre 'b', spawn gBrowser)
>                  , (pre 'e', spawn gEmacs)
>                  , (pre 'p', spawn gPass)
>                  , (pre 'i', spawn gIrc)
>                  , (pre 's', spawn gSound)
>                  , (pre 'f', spawn gFiles)
>                  , (pre 'n', spawn gQuickNote)
> 
>                  , ("M-S-<Return>", windows W.swapMaster)
> 
>                  , ("M-m", sendMessage Shrink)
>                  , ("M-w", sendMessage Expand)
> 
>                  , ("M-S-m", sendMessage $ IncMasterN    1)
>                  , ("M-S-w", sendMessage $ IncMasterN $ -1)
> 
>                  , ("M-p"  , (viewScreen $ P 5) >> banish LowerRight)
>                  , ("M-S-p", (sendToScreen $ P 5))
> 
>                  -- Can keybinds be dependent on current layout?
>                  , ("M-l", windowGo R False >> banish LowerRight)
>                  , ("M-h", windowGo L False >> banish LowerRight)
>                  , ("M-k", windowGo U False >> banish LowerRight)
>                  , ("M-j", windowGo D False >> banish LowerRight)
> 
>                  , ("M-S-l", windowSwap R False)
>                  , ("M-S-h", windowSwap L False)
>                  , ("M-S-k", windowSwap U False)
>                  , ("M-S-j", windowSwap D False)
> 
>                  , ("M-s", toggleWS)
> 
>                  -- Do I even want these?
>                  -- especcially if they don't work on a per-screen basis
>                  -- Possibly write some own which works together with IndependentScreens
>                  , ("M-g"  , prevScreen)
>                  , ("M-c"  , nextScreen)
>                  --, ("M-S-g", shiftPrevScreen)
>                  --, ("M-S-c", shiftNextScreen)
> 
>                  , ("M-y", shellPrompt myXPConfig { autoComplete = Nothing
>                                                   , searchPredicate = isInfixOf } )
>                  , ("M-x", xmonadPrompt myXPConfig { autoComplete = Nothing })
> 
>                  , ("M-q", spawn xmonadRe)
>                  , ("M-t", withFocused $ windows . W.sink)
> 
>                  , ("M-a", sendMessage $ IncMasterN 1)
>                  , ("M-.", sendMessage $ IncMasterN (- 1))
> 
>                  , ("M-S-c", kill)
>                  , ("M-n", sendMessage NextLayout)
> 
>                  , ("M-<Space> M-<Space>", selectWorkspace myXPConfig)
>                  , ("M-<Space> <Space>", selectWorkspace myXPConfig)
> 
>                  , ("M-<Space> d", removeWorkspace)
>                  , ("M-<Space> M-d", removeEmptyWorkspace)
>                  , ("M-<Space> a", addWorkspacePrompt myXPConfig)
>                  , ("M-<Space> s", withWorkspace myXPConfig (windows . W.shift))
>                  -- , ("M-<Space> S-s", inputPromptWithCompl myXPConfig "Shift and go" >>= shiftAndGo)
> 
>                  , ("M-<Space> r", renameWorkspace myXPConfig { autoComplete = Nothing })
> 
>                  ] where pre = \a -> "M-f " ++ [a]
>                          gBrowser   = "google-chrome"
>                          gEmacs     = "emacsclient -c"
>                          gMail      = "thunderbird"
>                          gPass      = "passmenu"
>                          gIrc       = "xterm -e ssh irc -t screen -x"
>                          gSound     = "pavucontrol"
>                          gFiles     = "thunar"
>                          gQuickNote = "gvim +'setlocal buftype=nofile' -n" --s <(echo -n i)"
> 
>                          xmonadRe = "xmonad --recompile; xmonad --restart"
> 
>                          myXPConfig = defaultXPConfig
>                              { position = Top -- CenteredAt
>                              , historySize = 1000
>                              , autoComplete = Just 1
>                              , font = myFont
>                              -- , searchPredicate = isInfixOf
>                              }
> 
>                          decoration = dwmStyle shrinkText def
> 
>                          tallThreshold = 1200
>                          wideLayouts =  OneBig (3/4) (3/4) ||| Tall 1 (3/100) (3/5) ||| GridRatio (4/3)
>                          tallLayouts = Dishes 1 (1/4) ||| GridRatio (4/3)
> 
> 
