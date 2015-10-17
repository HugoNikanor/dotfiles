import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myTerminal = "gnome-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myModMask = mod4Mask


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm , xK_w) , sendMessage (IncMasterN 1))
    , ((modm , xK_v) , sendMessage (IncMasterN (-1))) 
    ]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_o, xK_e, xK_u] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main = xmonad defaults

defaults = defaultConfig
    { terminal = myTerminal
	, focusFollowsMouse = myFocusFollowsMouse
    , modMask  = myModMask
    , keys = myKeys
    }

