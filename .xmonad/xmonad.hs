import XMonad

import XMonad.Actions.DynamicProjects
import XMonad.Actions.DwmPromote
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
-- import XMonad.Actions.UpdateFocus

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.SetWMName

import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import qualified XMonad.Layout.NoBorders as BO
import XMonad.Layout.Spacing
import XMonad.Layout.MultiColumns
import XMonad.Layout.Gaps

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

import XMonad.Util.Run (spawnPipe, runProcessWithInput, runInTerm, safeSpawn)
-- import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook)
import XMonad.Util.NamedScratchpad

import XMonad.Wallpaper

import Graphics.X11.ExtraTypes.XF86

import Data.Char (isSpace)
import Data.Monoid

import System.IO
import System.Exit

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

myTerminal :: [Char]
myTerminal = "kitty"

myTmux :: [Char]
myTmux = "kitty tmux"

myScreenlocker :: [Char]
myScreenlocker = "/usr/bin/slock"

myTitleLength :: Int
myTitleLength = 40

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 4

myFocusedBorderColor :: [Char]
myFocusedBorderColor = red02

myNormalBorderColor :: [Char]
myNormalBorderColor = cyan02

cyan01   = "#586e75"  -- bluish cyan hue, marble blue -- UNUSED
cyan02   = "#073642"  -- bluish cyan hue, elephant
cyan03   = "#002b36"  -- bluish cyan hue, firefly
red      = "#dc322f"  -- red hue, rose madder
red02    = "#A52A2A"  -- red hue, red brown
red03    = "#2F2F2F"  -- red hue, thunder
yellow   = "#D5CD6A"  -- orangy yellow, sand, pastel
yellow02 = "#F0E0AF"  -- orange-yellow, colonial white, pastel
blue     = "#268bd2"  -- cyan-blue hue, blue ivy
green    = "#98C379"

active       = blue
activeWarn   = red
inactive     = cyan02
focusColor   = blue
unfocusColor = cyan02


myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = inactive
    , activeBorderColor     = active
    , inactiveBorderColor   = inactive
    , activeTextColor       = cyan03
    , inactiveTextColor     = inactive
    }
              
myFont :: [Char]
myFont = "xft:Iosevka"

myScratchpads = [
  NS "bash" "urxvt -name notes -e /bin/bash" (appName =? "notes") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]



projects :: [Project]
projects =
  [ Project { projectName      = "1"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do safeSpawn "firefox-esr" []
            }
  , Project { projectName      = "2"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do runOrRaiseMaster "emacs" (className =? "Emacs")
            }
  , Project { projectName      = "3"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn myTmux
            }
  , Project { projectName      = "7"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "kitty --title mutt neomutt"
            }
  , Project { projectName      = "8"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "~/bin/signal-desktop"
            }
  , Project { projectName      = "9"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "kitty --title jrnl my_jrnl"
            }
  ]




myLayout = simpleTabbed ||| tiled ||| spacedTiled ||| spacedGrid ||| gappedSpacedGrid2 ||| Grid ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall 1 (10/100) (1/2)
    spacedTiled = spacingWithEdge 8 $ tiled
    spacedGrid = spacingWithEdge 8 $ Grid
    -- gappedSpacedGrid = spacing 10 $ gaps [(U,8), (D, 8), (L, 8), (R,8)] $ Grid
    gappedSpacedGrid2 = spacing 100 $ gaps [(U,10), (D, 10), (L, 10), (R, 200)] $ tiled
    -- gappedSpacedGrid2 = spacing 20 $ gaps [(U,50), (D, 50), (L, 300), (R,300)] $ Grid
    simpleTabbed = tabbed shrinkText myTabTheme

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen                        --> doFullFloat
      , className =? "Tor Browser"        --> doShift "1"
      , className =? "Firefox-esr"        --> doShift "1"
      , className =? "Emacs"              --> doShift "2"
      , title     =? "mutt"               --> doShift "7"
      , title     =? "profanity"          --> doShift "7"
      , className =? "Signal"             --> doShift "8"
      , className =? "Signal Beta"        --> doShift "8"
      , title     =? "jrnl"               --> doShift "9"
      , className =? "Navigator"          --> doFloat
      , className =? "Viewnior"           --> doFloat
      , className =? "Pinentry"           --> doCenterFloat
      , className =? "vlc"                --> doFullFloat
      , className =? "mpv"                --> doFullFloat
      , className =? "Arandr"             --> doCenterFloat
      , className =? "Evince"             --> doRectFloat (W.RationalRect 0.2 0.05 0.6 0.9)
      , className =? "Pavucontrol"        --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)
      , stringProperty "WM_WINDOW_ROLE" =? "pop-up"               --> doFloat
      , stringProperty "WM_WINDOW_ROLE" =? "gimp-message-dialog"  --> doFloat
      , isDialog                                                  --> doFloat
      , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat (W.RationalRect 0.2 0.1 0.6 0.8)
    ]
       <+> namedScratchpadManageHook myScratchpads
    -- <+> scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)
myWorkspaces :: [[Char]]
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myPP :: PP
myPP = def { ppTitle   = xmobarColor blue      "" . shorten myTitleLength
           , ppCurrent = xmobarColor green    "" . wrap "[" "]"
           , ppHidden  = xmobarColor active    "" . noScratchPad
           , ppLayout  = xmobarColor blue      ""
           , ppSep     = " :: "
           , ppWsSep   = " "
           , ppOrder   = \(ws:___) -> [ws]
           }
  where
    noScratchPad ws = if ws == "NSP"
                      then ""
                      else ws

myXPConfig :: XPConfig
myXPConfig = def
             { font              = myFont
             , bgColor           = red02
             , fgColor           = yellow
             , bgHLight          = red03
             , fgHLight          = yellow02
             , promptBorderWidth = 0
             , position          = Top
             , height            = 20
             , historySize       = 50
             , historyFilter     = deleteConsecutive
             }

myCalcConfig :: XPConfig
myCalcConfig = def
               { font                = myFont
               , bgColor             = red02
               , fgColor             = yellow
               , promptBorderWidth   = 0
               , position            = Top
               }

-- Calculator
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input -> liftIO(runProcessWithInput "concalc" [input] "") >>= calcPrompt c
  where
    trim  = f . f
      where f = reverse . dropWhile isSpace


myModMask :: KeyMask
myModMask = mod4Mask

-- showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm              , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm              , xK_s     ), spawn "dmenu_run -p $ -nf white -sb brown -sf yellow")

    -- close focused window
    , ((modm              , xK_BackSpace ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp)

    -- Emacs
    , ((modm,               xK_e     ), runOrRaise "emacs" (className =? "Emacs"))
    
    -- Emacs
    , ((modm,               xK_f     ), runOrRaise "firefox" (className =? "Firefox-esr"))

    -- Mutt
    , ((modm,               xK_m     ), raiseMaybe (spawn "kitty --title mutt neomutt") (title =? "mutt"))

    -- Profanity
    , ((modm,               xK_p     ), raiseMaybe (spawn "kitty --title profanity profanity") (title =? "profanity"))

    -- Pavucontrol
    , ((modm,               xK_v     ), runOrRaiseMaster "pavucontrol" (className =? "Pavucontrol"))

    -- Volume Mute
    , ((0  , xF86XK_AudioMute        ), spawn "pa_switch --sink-mute-toggle")

    -- Volume Up
    , ((0  , xF86XK_AudioRaiseVolume ), spawn "pa_switch --volume-up")

    -- Volume Down
    , ((0  , xF86XK_AudioLowerVolume ), spawn "pa_switch --volume-down")

    -- Mic Toggle
    , ((0  , 0x1008FFB2              ), spawn "pa_switch --mic-toggle")

    -- Brightness Up
    , ((0  , xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 10")

    -- Brightness Down
    , ((0  , xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm              , xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm              , xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm              , xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Lock the screen using command specified by myScreenlocker
    , ((0                 , xK_F12   ), spawn myScreenlocker)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Push the master window to the next window in the stack
    , ((modm .|. shiftMask, xK_Return), dwmpromote)

    -- Pop open a dmenu to poweroff/reboot/suspend/lock
    , ((modm              , xK_x     ), spawn "dmenu_shutdown")

    -- Pop open a dmenu to pass
    , ((modm              , xK_u     ), spawn "passmenu")

    -- Pop tiny terminal window via Scratchpad
    -- , ((modm              , xK_grave ), scratchpadSpawnAction def {terminal = "urxvt"})
    , ((modm              , xK_grave ), namedScratchpadAction myScratchpads "bash")

    -- Shell Prompt to run a shell command
    , ((modm .|. controlMask, xK_x   ), shellPrompt myXPConfig)

    -- Calculator Prompt to run concalc
    , ((modm              , xK_c     ), calcPrompt myCalcConfig "concalc")
    
    ]
    ++

  
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.config/xmobar/xmobarrc"
    setRandomWallpaper ["$HOME/syncthing/wallpapers"]
    xmonad $ docks $ ewmhFullscreen . ewmh $ dynamicProjects projects def
    -- xmonad $ ewmh $ def
        { workspaces         = myWorkspaces
        , terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor
        , normalBorderColor  = myNormalBorderColor
        , startupHook        = setWMName "LG3D"
        , modMask            = myModMask
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , handleEventHook    = perWindowKbdLayout
        , manageHook = manageDocks <+> myManageHook
                        <+> manageHook def
        , layoutHook = BO.lessBorders BO.Never $ avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        }
