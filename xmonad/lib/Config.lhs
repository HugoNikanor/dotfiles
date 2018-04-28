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

A list of all(?) X keynames are in =/usr/include/X11/XF86keysym.h=,
these have to have their first letter downcased. For some media keys
extra imports have to be done here.

The X key names of the swedish letters is

> å = xK_aring
> ä = xK_adiaeresis
> ö = xK_odiaeresis

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

> xmonadRe = "xmonad --recompile; xmonad --restart"

> myXPConfig = defaultXPConfig
>              { position = Top -- CenteredAt
>              , historySize = 1000
>              , autoComplete = Just 1
>              , font = myFont
>                       -- , searchPredicate = isInfixOf
>              }

> decoration = dwmStyle shrinkText def

> tallThreshold = 1200
> wideLayouts =  OneBig (3/4) (3/4) ||| Tall 1 (3/100) (3/5) ||| GridRatio (4/3)
> tallLayouts = Dishes 1 (1/4) ||| GridRatio (4/3)

------------------------------------------------------------

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
>                         -> [((KeyMask, KeySym), X ())]

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

Footnote:
Choose one of these, depending on the current monitor setup.
(`x' declared as binding for /no monitor/)

> x = -1 :: Int

> -- (a, b, c) = (x, 0, x)
> -- (d, e, f) = (x, 1, 2)

> (a, b, c) = (x, 0, x)
> (d, e, f) = (1, 2, 3)

> monitorKeys conf = bindWithAndWithoutShift
>   (\i -> (viewScreen $ P i) >> banish LowerRight)
>   (\i -> (sendToScreen $ P i))
>   [ (xK_adiaeresis, a)
>   , (xK_odiaeresis, b)
>   , (xK_p, c)
>   , (xK_o, d)
>   , (xK_e, e)
>   , (xK_u, f) ]
>   conf

----

> movementKeys conf = bindWithAndWithoutShift
>   (\d -> windowGo d False >> banish LowerRight)
>   (\d -> windowSwap d False)
>   [ (xK_h, L)
>   , (xK_j, D)
>   , (xK_k, U)
>   , (xK_l, R) ]
>   conf

----

> otherKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
> otherKeys conf@(XConfig {XMonad.modMask = modm}) = 
>     [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
>     , ((modm, xK_Tab ), windows W.focusDown)
>     , ((modm .|. shiftMask, xK_Tab ), windows W.focusUp)
>     -- This doesn't work in the easy config, for some reason
>     , ((modm, xK_t), withFocused $ windows . W.sink)
>     ]

Finnaly add all the parts together

footnote:
The =conf@(XConfig {XMonad.modMask = modm})=
part means that conf is sent as a parameter, and that
=modm= is bound as if sent by pattern matching!

> myKeys conf@(XConfig {XMonad.modMask = modm}) =
>   M.fromList . mconcat . fmap ($ conf)
>     $ [ otherKeys
>       , monitorKeys
>       , movementKeys ]

----

The following keybinds are managed by EZ-config.

> pre = \a -> "M-f " ++ [a]

> ezkeys =
>   [ (pre 'b', spawn "google-chrome")
>   , (pre 'e', spawn "emacsclient -c")
>   , (pre 'p', spawn "passmenu")
>   , (pre 'i', spawn "xterm -e ssh irc -t screen -x")
>   , (pre 's', spawn "pavucontrol")
>   , (pre 'f', spawn "thunar")
>   , (pre 'n', spawn "gvim +'setlocal buftype=nofile' -n")
>   
>   , ("M-S-<Return>", windows W.swapMaster)
>   
>   , ("M-m", sendMessage Shrink)
>   , ("M-w", sendMessage Expand)
>   
>   , ("M-S-m", sendMessage $ IncMasterN    1)
>   , ("M-S-w", sendMessage $ IncMasterN $ -1)
>   
>   , ("M-p"  , (viewScreen $ P 5) >> banish LowerRight)
>   , ("M-S-p", (sendToScreen $ P 5))
>   
>   
>   , ("M-s", toggleWS)
>   
>     -- Do I even want these?
>     -- especcially if they don't work on a per-screen basis
>     -- Possibly write some own which works together with IndependentScreens
>   , ("M-g"  , prevScreen)
>   , ("M-c"  , nextScreen)
>     --, ("M-S-g", shiftPrevScreen)
>     --, ("M-S-c", shiftNextScreen)
>   
>   , ("M-y", shellPrompt myXPConfig { autoComplete = Nothing
>                                    , searchPredicate = isInfixOf } )
>   , ("M-x", xmonadPrompt myXPConfig { autoComplete = Nothing })
>   
>   , ("M-q", spawn xmonadRe)
>   , ("M-t", withFocused $ windows . W.sink)
>   
>   , ("M-a", sendMessage $ IncMasterN 1)
>   , ("M-.", sendMessage $ IncMasterN (- 1))
>   
>   , ("M-S-c", kill)
>   , ("M-n", sendMessage NextLayout)
>   
>   , ("M-<Space> M-<Space>", selectWorkspace myXPConfig)
>   , ("M-<Space> <Space>", selectWorkspace myXPConfig)
>   
>   , ("M-<Space> d", removeWorkspace)
>   , ("M-<Space> M-d", removeEmptyWorkspace)
>   , ("M-<Space> a", addWorkspacePrompt myXPConfig)
>   , ("M-<Space> s", withWorkspace myXPConfig (windows . W.shift))
>     -- , ("M-<Space> S-s", inputPromptWithCompl myXPConfig "Shift and go" >>= shiftAndGo)
>   
>   , ("M-<Space> r", renameWorkspace myXPConfig { autoComplete = Nothing })
>   
>   ]

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
>     xmonad $ def { modMask = mod4Mask
>                  , clickJustFocuses  = False
>                  , focusFollowsMouse = False
>                  , keys     = myKeys
>                  , terminal = termCommand
>                  , layoutHook = decoration $ ifWider tallThreshold wideLayouts tallLayouts
>                  , manageHook = myManageHook <+> insertPosition Below Newer
>                  , workspaces = ["term", "web"]
>                  , normalBorderColor  = "#1d1f21"
>                  , focusedBorderColor = "#FF0000"
>                  } `additionalKeysP` ezkeys

Appendix:
=========

In the key binding section this function is used.
It should be self explanitory enough (...). Here
it's used to lower the amount of boilerplate needed.

> bindWithAndWithoutShift noShift withShift binds
>   conf@(XConfig {XMonad.modMask = modm}) =
>   [ ((modm .|. m, k), f d)
>   | (k, d) <- binds
>   , (m, f) <- [ (0,         noShift)
>               , (shiftMask, withShift) ]]
