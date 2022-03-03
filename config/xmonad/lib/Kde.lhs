> module Kde ( kdeMain ) where

> import XMonad

> import XMonad.Config.Kde (kde4Config)
> import XMonad.Layout.Spacing (spacing)
> import XMonad.Layout.LayoutHints (layoutHints)

Alternative config for using xmonad as window manager for KDE plasma.
To enable, set window manager as follows,
    $ cat ~/.config/plasma-workspace/env/set_window_manager.sh
    export KDEWM=/usr/bin/xmonad
and change main to point to kdemain.

Preferable our main function should switch between a standalone
configuration, and this KDE-enabled configuration depending on the KDE
environment.

Also note that this is mostly untested, due to KDE not being overjoyed
with a tiling window manager.

manageHook
isDialog --> doSmartFloat

> kdemain = do
>   nScreens    <- countScreens
>   xmonad $ docks $ kde4Config
>         { modMask = mod4Mask
>         -- , logHook = myLogHook xmproc loghookExtras
>         , clickJustFocuses  = False
>         , focusFollowsMouse = True
>         , keys = \conf -> M.fromList . mconcat . fmap ($ conf) $
>               [ otherKeys
>               , monitorKeys
>               -- , movementKeys
>               -- , volumeKeys
>               ]
>         -- , handleEventHook = handleEventHook def <+> ewmhDesktopEventHook ActiveWindow
>         , layoutHook = avoidStruts
>                      $ windowNavigation
>                      -- $ S.subTabbed
>                      $ B.boringWindows
>                      $ layoutHints
>                      $ spacing 5
>                      $ myLayouts
>         , manageHook = manageHook kde4Config <+> myManageHook -- <+> insertPosition Below Newer
>         , normalBorderColor  = "#1d1f21"
>         , focusedBorderColor = "#FF0000"
>         -- , handleEventHook = handleEvent
>         }
