import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer

import Control.Arrow
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal"
        , modMask = mod4Mask -- set the mod key to the windows key
        , startupHook = setWMName "LG3D"
        , manageHook = composeAll
             [ manageHook gnomeConfig
             , isFullscreen --> doFullFloat
             , title =? "VLC (XVideo output)" --> doFullFloat
             , className =? "Gcalctool" --> doCenterFloat
             , className =? "Pidgin" --> doCenterFloat
             , className =? "Skype" --> doCenterFloat
             ]
        }
        `additionalKeysP`
                 [ ("M-m", spawn "echo")
                 , ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
                 , ("M-S-q", spawn "gnome-session-save --gui --logout-dialog")
                 , ("M-f", fullFloatFocused)
                  ]


fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
