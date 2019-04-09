> module Config where

> import System.IO (hPutStrLn)
> import Network.HostName (HostName, getHostName)
> import Data.List (isInfixOf)

> import XMonad

> import XMonad.Actions.PhysicalScreens
>     (viewScreen, PhysicalScreen (P), sendToScreen)
> import XMonad.Actions.DynamicWorkspaces
>     ( selectWorkspace
>     , removeWorkspace
>     , removeEmptyWorkspace
>     , addWorkspacePrompt
>     , withWorkspace
>     , renameWorkspace )
> import XMonad.Actions.CycleWS (prevScreen, nextScreen, toggleWS)
> import XMonad.Actions.Warp (banish, Corner (LowerRight))
> import XMonad.Actions.Navigation2D (windowGo, windowSwap)

> import XMonad.Hooks.InsertPosition
>     (Position (Below), Focus (Newer), insertPosition)
> import XMonad.Hooks.ManageHelpers (isDialog)
> import XMonad.Hooks.Place
> import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docks)
> import XMonad.Hooks.DynamicLog

> import XMonad.Layout.Grid (Grid (Grid, GridRatio))
> import XMonad.Layout.PerScreen (ifWider)
> import XMonad.Layout.Dishes (Dishes (Dishes))
> import XMonad.Layout.Decoration (shrinkText)
> import XMonad.Layout.DwmStyle (dwmStyle)
> import XMonad.Layout.OneBig (OneBig (OneBig))
> import XMonad.Layout.Spiral (spiral)
> import XMonad.Layout.WindowNavigation (windowNavigation)

IndependentScreens is the library which should allow things
to happen at points not currently in focus.

> import XMonad.Layout.IndependentScreens (countScreens)

> import XMonad.Prompt (XPConfig (..), XPPosition (Top))
> import XMonad.Prompt.Input (inputPrompt)
> import XMonad.Prompt.Shell (shellPrompt)
> import XMonad.Prompt.XMonad (xmonadPrompt)

> import XMonad.Util.Types (Direction2D (U, D, L, R))
> import XMonad.Util.EZConfig
> import XMonad.Util.Run (spawnPipe)

> import qualified Data.Map as M
> import qualified XMonad.StackSet as W
> import qualified XMonad.Layout.BoringWindows as B
> import qualified XMonad.Layout.SubLayouts as S



A list of all(?) X keynames are in =/usr/include/X11/XF86keysym.h=,
these have to have their first letter downcased. For some media keys
extra imports have to be done here.

The X key names of the swedish letters is

> xK_å = xK_aring
> xK_ä = xK_adiaeresis
> xK_ö = xK_odiaeresis

------------------------------------------------------------

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

> spawnToWorkspace :: String -> String -> X ()
> spawnToWorkspace workspace program = do
>     spawn program
>     windows $ W.greedyView workspace


These should have a propper place somewhere.

> restartXMonad = spawn "xmonad --recompile; xmonad --restart"

> myXPConfig = def
>   { position = Top -- CenteredAt
>   , historySize = 100
>   , autoComplete = Just 1
>   -- , font = myFont
>   -- , bgColor = "black"
>   -- , searchPredicate = isInfixOf
>   , bgColor = bgColor'
>   , fgColor = fgColor'
>   , fgHLight = "yellow"
>   , bgHLight = bgColor'
>   , promptBorderWidth = 0
>   }

> -- decoration = dwmStyle shrinkText def

The window layouts are split into two parts, those for wide
screens and those for tall screens. Currently I only check
a `tallThreshold', but this obviously fails for higher DPI
screens.

> myLayouts = ifWider tallThreshold wideLayouts tallLayouts
>   where tallThreshold = 1200
>         wideLayouts = Tall 1 (3/100) (3/5)
>                   ||| spiral (1/2)
>                   ||| GridRatio (4/3)
>         tallLayouts = Dishes 1 (1/4)
>                   ||| GridRatio (4/3)

Workspace ID is just the name of the workspace.

> getCurrentWorkspaceId :: X WorkspaceId
> getCurrentWorkspaceId = gets (W.currentTag . windowset)



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

> (a, b, c) = (0, 1, 2)
> (d, e, f) = (3, 4, 5)

> monitorKeys = bindWithAndWithoutShift
>   (\i -> (viewScreen def $ P i) >> banish LowerRight)
>   (\i -> (sendToScreen def $ P i))
>   [ (xK_ä, a)
>   , (xK_ö, b)
>   , (xK_p, c)
>   , (xK_o, d)
>   , (xK_e, e)
>   , (xK_u, f) ]



This adds directional movement, instead of the default 1D movement.
Especially good on larger screens.

> movementKeys = bindWithAndWithoutShift
>   (\d -> windowGo d False >> banish LowerRight)
>   (\d -> windowSwap d False)
>   [ (xK_h, L)
>   , (xK_j, D)
>   , (xK_k, U)
>   , (xK_l, R) ]



> otherKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
> otherKeys conf@(XConfig {XMonad.modMask = modm}) = 
>     [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
>     , ((modm, xK_Tab ), S.onGroup W.focusDown')
>     , ((modm .|. shiftMask, xK_Tab ), S.onGroup W.focusUp')
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
>   , ("M-j", B.focusDown)
>   , ("M-k", B.focusUp)
>   , ("M-S-j", windows W.swapDown)
>   , ("M-S-k", windows W.swapUp)
>   , ("M-y", spawn "passmenu")
>
>   , ("M-<Return>", windows W.swapMaster)
>   
>   , ("M-m", sendMessage Shrink)
>   , ("M-w", sendMessage Expand)
>   , ("M-S-m", sendMessage $ IncMasterN    1)
>   , ("M-S-w", sendMessage $ IncMasterN $ -1)
>   , ("M-l", S.onGroup W.focusDown')
>   , ("M-h", S.onGroup W.focusUp')
>
>   , ("M-<Space> m", withFocused (sendMessage . S.MergeAll))
>   , ("M-<Space> u", withFocused (sendMessage . S.UnMerge))

  Directions are inverted to what is "normal". This since I want to think about
  it like a lasso going out in that direction.

>   , ("M-<Space> j", sendMessage $ S.pullGroup D)
>   , ("M-<Space> k", sendMessage $ S.pullGroup U)
>   , ("M-<Space> h", sendMessage $ S.pullGroup R)
>   , ("M-<Space> l", sendMessage $ S.pullGroup L)
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
>   , ("M-p", shellPrompt myXPConfig { autoComplete = Nothing
>                                    , searchPredicate = isInfixOf } )
>   , ("M-x", xmonadPrompt myXPConfig { autoComplete = Nothing })
>   
>   , ("M-q", restartXMonad)
>   , ("M-t", withFocused $ windows . W.sink)
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
>   -- , ("M-<Space> S-s", inputPromptWithCompl myXPConfig "Shift and go" >>= shiftAndGo)
>   
>   , ("M-<Space> r", renameWorkspace myXPConfig { autoComplete = Nothing })
>   ]



`placeHook' places floating windows at interesting places.
This tries to place windows close to the center of the
screen, which is what I want.

> placeNearCenter = placeHook (withGaps (10, 10, 10, 10) (smart (0.5,0.5)))

Compositioning of rules go from right to left (I think), the
`doFloat' is just so that the window can actually be placeded
smartly after.

> doSmartFloat = placeNearCenter <+> doFloat

> -- "Steam - News (1 of 2)"

> myManageHook = composeAll
>     [ className =? "Gvim" --> doSmartFloat
>     , className =? "Pinentry" --> doSmartFloat
>     , className =? "Floating" --> doSmartFloat
>     , className =? "Gimp" --> doShift "gimp"
>     , className =? "Steam" --> doShift "steam"
>     , className =? "Xmessage" --> doSmartFloat
>     , isDialog --> doSmartFloat
>
>     -- Who doesn't this work!
>     -- , className =? "Gimp" <&&> (appName =? "Toolbox" <||> title =? "Toolbox") --> doFloat ]
>     --, className =? "Gimp" --> doFloat ]
>     , title =? "Toolbox" --> doSmartFloat ]

> shiftAndGo :: Maybe String -> X ()
> shiftAndGo Nothing = return ()
> shiftAndGo (Just str) =
>     windows $ do
>         W.shift $ str
>         W.greedyView $ str



Color config borrowed from my Termite config .

> foreground      = "#c5c8c6"
> foreground_bold = "#c5c8c6"
> cursor          = "#c5c8c6"
> background      = "#1d1f21"

> fgColor' = foreground
> bgColor' = "black"

Log hook borrowed from https://pastebin.com/Pt8LCprY.

> -- colorFunc = dzenColor
> -- funcPP = dzenPP
> colorFunc = xmobarColor
> funcPP = xmobarPP
> myLogHook handle = dynamicLogWithPP $ funcPP
>   { ppCurrent = \str -> colorFunc "yellow" bgColor' $ "[" ++ str ++ "]"
>   , ppTitle = shorten 100
>   , ppWsSep = " "
>   , ppSep = " | "
>   , ppOutput = hPutStrLn handle
>   }



> main = do
>     -- termCommand <- getTerminalCommand <$> getHostName
>     let termCommand = "termite"
>     nScreens    <- countScreens
>     xmproc      <- spawnPipe "xmobar"
>     xmonad $ docks def
>         { modMask = mod4Mask
>         , logHook = myLogHook xmproc
>         , clickJustFocuses  = False
>         , focusFollowsMouse = True
>         , keys     = myKeys
>         , terminal = termCommand
>         , layoutHook = avoidStruts
>                      $ windowNavigation
>                      $ S.subTabbed
>                      $ B.boringWindows
>                      $ myLayouts
>         , manageHook = manageDocks <+> myManageHook <+> insertPosition Below Newer
>         , workspaces = ["term", "web"] ++ map show [3 .. nScreens]
>         , normalBorderColor  = "#1d1f21"
>         , focusedBorderColor = "#FF0000"
>         } `additionalKeysP` ezkeys


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
