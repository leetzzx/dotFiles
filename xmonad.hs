import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageDocks
import Cmm (GlobalReg(PicBaseReg))
main :: IO()
main = xmonad $ ewmhFullscreen $ ewmh $ docks $ def {
  modMask = mod4Mask
  ,layoutHook = spacingWithEdge 6 $  gaps [(U,1), (R,1), (L,1),(D,1)] $ avoidStruts $ Tall 1 (3/100) (1/2) ||| Full   ||| noBorders (Full ||| tabbed shrinkText def ||| Accordion )
  , startupHook = startup
  , manageHook  = manageHook def <+> manageDocks
  , terminal = "kitty"
} 
  `additionalKeysP`
    [ 
    ("M-c"  , spawn "google-chrome-stable" )
    ,("M-e"  , spawn "shutdown now" )
    ,("M-S-u"  , spawn "lightctl inc" )
    ,("M-S-d"  , spawn "lightctl dec" )
--    ,("M-S-i"  , spawn "amixer -q sset Master 2%+" )
--    ,("M-S-o"  , spawn "amixer -q sset Master 2%-" )
    ,("M-S-m"  , spawn "amixer -q sset Master,0 toggle" )
    ,("M-S-z"  , spawn "i3lock-fancy" )
    ,("M-d"  , spawn "rofi -show run" )
    ,("M-w"  , spawn "rofi -show window" )
    , ("M-'"  , spawn "kitty" )
    , ("M-S-e"  , spawn "killall xmonad-x86_64-linux" )
    ]
  `removeKeysP`
  [
     "M-q"
    ,"M-p"
    ,"M-S-q"
  ]


startup::X()
startup = do 
   spawn "tap-to-click.sh"
   spawn "feh --bg-scale ~/Downloads/bg_4X.jpeg"
   spawn "fcitx"
   spawn "picom -r 5"
--   spawn "wallset --video ~/Downloads/video.mp4"
   spawn "tint2"
   spawn "nm-applet"
   spawn "volumeicon"
