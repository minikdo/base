import XMonad

import XMonad.Actions.DynamicProjects
import XMonad.Actions.DwmPromote
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.PerWindowKbdLayout
import XMonad.Hooks.SetWMName

import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

import XMonad.Util.Run (spawnPipe, runProcessWithInput, runInTerm, safeSpawn)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook)

import XMonad.Wallpaper

import Graphics.X11.ExtraTypes.XF86

import Data.Char (isSpace)
import Data.Monoid

import System.IO
import System.Exit

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

myTerminal :: [Char]
myTerminal = "st"

myTmux :: [Char]
myTmux = "st -e tmux"

myScreenlocker :: [Char]
myScreenlocker = "/usr/bin/slock"

myYAScreenlocker :: [Char]
myYAScreenlocker = "/usr/bin/xtrlock"

myTitleLength :: Int
myTitleLength = 40

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: Dimension
myBorderWidth = 1

myFocusedBorderColor :: [Char]
myFocusedBorderColor = active

base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
base00  = "#657b83"
base01  = "#586e75"
base02  = "#073642"
base03  = "#002b36"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }
              
myFont :: [Char]
myFont = "xft:Inconsolata-zi4"

projects :: [Project]
projects =
  [ Project { projectName      = "1"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do runOrRaiseMaster "torbrowser-launcher" (className =? "Tor Browser")
                                           safeSpawn "firefox" []
            }
  , Project { projectName      = "2"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do runOrRaiseMaster "emacs" (className =? "Emacs")
            }
  , Project { projectName      = "3"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn myTmux
            }
  -- , Project { projectName      = "7"
            -- , projectDirectory = "~/"
            -- , projectStartHook = Just $ do spawn "LC_ALL=pl_PL.UTF-8 signal-desktop"
            -- }
  -- , Project { projectName      = "8"
            -- , projectDirectory = "~/"
            -- , projectStartHook = Just $ do spawn "st -t mutt ~/bin/my_mutt"
                                           -- spawn "st -t profanity ~/my_profanity"
            -- }
  , Project { projectName      = "9"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "st -t jrnl ~/bin/my_tmux.sh"
            }
  ]

myLayout = simpleTabbed ||| tiled ||| gappedSpacedGrid ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall 1 (10/100) (1/2)
    -- spacedGrid = spacingWithEdge 10 $ Grid
    gappedSpacedGrid = spacing 10 $ gaps [(U,10), (D, 10), (L, 10), (R,320)] $ Grid
    simpleTabbed = tabbed shrinkText myTabTheme

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen                        --> doFullFloat
      , className =? "Tor Browser"        --> doShift "1"
      , className =? "Firefox-esr"        --> doShift "1"
      , className =? "Emacs"              --> doShift "2"
      , className =? "Element"            --> doShift "7"
      , title     =? "mutt"               --> doShift "8"
      , title     =? "profanity"          --> doShift "8"
      , className =? "Navigator"          --> doFloat
      , className =? "Viewnior"           --> doFullFloat
      , className =? "Pinentry"           --> doCenterFloat
      , className =? "vlc"                --> doFullFloat
      , className =? "mpv"                --> doFullFloat
      , className =? "Arandr"             --> doCenterFloat
      , className =? "Evince"             --> doRectFloat (W.RationalRect 0.2 0.05 0.6 0.9)
      , className =? "Pavucontrol"        --> doCenterFloat
      , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
      , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doRectFloat (W.RationalRect 0.2 0.1 0.6 0.8)
    ] <+> scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)

myWorkspaces :: [[Char]]
myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

myPP :: PP
myPP = def { ppTitle   = xmobarColor blue      "" . shorten myTitleLength
           , ppCurrent = xmobarColor "#D5CD6A" "" . wrap "[" "]"
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
             , bgColor           = "#A52A2A"
             , fgColor           = "#D5CD6A"
             , bgHLight          = "#2F2F2F"
             , fgHLight          = "#F0E0AF"
             , promptBorderWidth = 0
             , position          = Top
             , height            = 20
             , historySize       = 50
             , historyFilter     = deleteConsecutive
             }

myCalcConfig :: XPConfig
myCalcConfig = def
               { font                = myFont
               , bgColor             = "#A52A2A"
               , fgColor             = "#D5CD6A"
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
    , ((modm              , xK_d     ), spawn "dmenu_run -p $ -nf white -sb brown -sf yellow")

    -- close focused window
    , ((modm              , xK_BackSpace ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,              xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp)

    -- Tor Browser
    -- , ((modm,               xK_i     ), runOrRaiseMaster "torbrowser-launcher" (className =? "Tor Browser"))

    -- Emacs Scratch
    -- , ((modm,               xK_s     ), spawn "e-scratch" )

    -- Mutt
    -- , ((modm,               xK_m     ), raiseMaybe (spawn "st -t mutt ~/bin/my_mutt") (title =? "mutt"))

    -- Profanity
    -- , ((modm,               xK_p     ), raiseMaybe (spawn "st -t profanity ~/bin/my_profanity") (title =? "profanity"))

    -- Pavucontrol
    , ((modm,               xK_v     ), runOrRaiseMaster "pavucontrol" (className =? "Pavucontrol"))

    -- Volume Up
    , ((0  , xF86XK_AudioRaiseVolume ), spawn "pa_vol_up")

    -- Volume Down
    , ((0  , xF86XK_AudioLowerVolume ), spawn "pa_vol_down")

    -- Mic Toggle
    , ((0  , 0x1008FFB2              ), spawn "pa_mic_toggle")

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

    -- Lock the screen using command specified by myYAScreenlocker
    , ((modm              , xK_F12   ), spawn myYAScreenlocker)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Push the master window to the next window in the stack
    , ((modm .|. shiftMask, xK_Return), dwmpromote)

    -- Pop open a dmenu to poweroff/reboot/suspend/lock
    , ((modm              , xK_z     ), spawn "dmenu_shutdown")

    -- Pop open a dmenu to pass
    , ((modm              , xK_u     ), spawn "passmenu")

    -- Pop tiny terminal window via Scratchpad
    , ((modm              , xK_grave ), scratchpadSpawnAction def {terminal = "urxvt"})

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
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.config/xmobar/xmobarrc"
    setRandomWallpaper ["$HOME/.local/share/wallpapers"]
    xmonad $ ewmh $ dynamicProjects projects def
    -- xmonad $ ewmh $ def
        { workspaces         = myWorkspaces
        , terminal           = myTmux
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor

        , startupHook        = setWMName "LG3D"
        , modMask            = myModMask
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        , handleEventHook    = mconcat
                               [ perWindowKbdLayout
                               , docksEventHook
                               , handleEventHook def <+> fullscreenEventHook ]

        , manageHook = manageDocks <+> myManageHook
                        <+> manageHook def
        , layoutHook = avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        }
