-- -*- mode: haskell -*-
-- {# LANGUAGE UnicodeSyntax #-}

import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import Text.Printf

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.ClickableWorkspaces
import qualified XMonad.Util.Hacks as Hacks

colorPrim    = "#407d52"
colorBg      = "#282a34"
colorGrey    = "#484854"
myTerminal   = "alacritty"
myRofi       = printf "rofi -show drun -theme-str '*{primary:%s;background:%s;}'" colorPrim colorBg
myWorkspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九"]

myLayouts =
      renamed [CutWordsLeft 1]
      $ spacingRaw False (Border 5 5 5 5) True (Border 5 5 5 5) True
      $ avoidStruts
      $ Tall 1 (3/100) (1/2)
      ||| ThreeCol 1 (3/100) (1/2)
      ||| Grid
      ||| spiral (6/7)
      ||| Full

myKeys :: [(String, X ())]
myKeys =
  -- XMonad Keys
  [ ("M-S-q", io exitSuccess)
  , ("M-S-r", spawn "xmonad --restart")
  , ("M-C-p", spawn "random_wallpaper.py")

  -- Window Keys
  , ("M-C-h", sendMessage Shrink)
  , ("M-C-l", sendMessage Expand)
  , ("M-C-i", sendMessage (IncMasterN (1)))
  , ("M-C-d", sendMessage (IncMasterN (-1)))
  -- , ("M-C-f", sendMessage (JumppToLayout "full"))

  -- Application Keys
  , ("M-S-<Return>", spawn (myTerminal))
  , ("M-S-p",        spawn "dmenu_run -p RUN")
  , ("M-p",          spawn (myRofi))
  ]

xmobarFont :: Int -> String -> String
xmobarFont n = wrap (printf "<fn=%d>" n) "</fn>"

main :: IO()
main = do
  xmproc <- spawnPipe "/home/lgs/.cache/xmobar/xmobar-x86_64-linux"
  xmonad $ docks $ ewmh $ def {
    modMask              = mod4Mask
    , workspaces         = myWorkspaces
    , terminal           = myTerminal
    , borderWidth        = 2
    , normalBorderColor  = colorGrey
    , focusedBorderColor = colorPrim
    , layoutHook         = myLayouts

    , manageHook = manageDocks <+> composeAll
      [ className =? "trayer"       --> doIgnore
      , className =? "stalonetray"  --> doIgnore]

    , startupHook = do
        setDefaultCursor xC_left_ptr

    -- hack for stalonetray, need xmonad-contrib-0.17.0.9
    , handleEventHook = handleEventHook def <> Hacks.trayPaddingXmobarEventHook (className =? "stalonetray") "_XMONAD_TRAYPAD"
    -- hack for trayer
    -- , handleEventHook = handleEventHook def <> Hacks.trayerAboveXmobarEventHook

    , logHook = clickablePP xmobarPP {
        ppOutput            = hPutStrLn xmproc
        , ppSep             = " | "
        , ppWsSep           = " "
        -- 当前工作区
        , ppCurrent         = xmobarColor colorPrim "" . xmobarFont 1 . wrap "[" "]"
        -- 可见但是没有焦点的工作区
        , ppVisible         = xmobarColor "grey" ""
        -- 有窗口但是没有焦点的工作区
        , ppHidden          = xmobarColor colorPrim "" . xmobarFont 1
        -- 没窗口没焦点的工作区
        , ppHiddenNoWindows = xmobarColor "grey" "" . xmobarFont 1
        -- 当前窗口标题
        , ppTitle           = xmobarColor colorPrim ""
      } >>= dynamicLogWithPP
  } `additionalKeysP` myKeys

