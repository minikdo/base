import XMonad

import XMonad.Actions.DwmPromote
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.PerWindowKbdLayout

import XMonad.Layout.OneBig
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid

import XMonad.Prompt
import XMonad.Prompt.Input
-- import XMonad.Prompt.MPD
import XMonad.Prompt.Shell

import XMonad.Util.Run (spawnPipe, runProcessWithInput, runInTerm)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook)

import XMonad.Wallpaper

import Graphics.X11.ExtraTypes.XF86

import Data.Char (isSpace)
import Data.Monoid

import System.IO
import System.Exit

import qualified Data.Map        as M
-- import qualified Network.MPD     as MPD
import qualified XMonad.StackSet as W

myTerminal :: [Char]
myTerminal = "urxvtc"

myScreenlocker :: [Char]
myScreenlocker = "/usr/bin/slock"

myYAScreenlocker :: [Char]
myYAScreenlocker = "/usr/bin/xtrlock"

myTitleLength :: Int
myTitleLength = 120

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 1

myFocusedBorderColor :: [Char]
myFocusedBorderColor = "#096ee2"

myLayout = tiled ||| Grid ||| Mirror tiled ||| OneBig (3/4) (3/4) ||| simpleTabbed ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall 1 (10/100) (1/2)

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen                        --> doFullFloat
      , className =? "Chromium"           --> doShift "1:web"
      , title     =? "mutt"               --> doShift "3:mutt"
      , className =? "Navigator"          --> doFloat
      , className =? "Tor Browser"        --> doFloat
      , className =? "Evince"             --> doFullFloat
      , className =? "Viewnior"           --> doFullFloat
      , className =? "vlc"                --> doFullFloat
      , className =? "mpv"                --> doFullFloat
      , className =? "Arandr"             --> doFloat
      , className =? "Pavucontrol"        --> doFloat
      , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
    ] <+> scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)

myWorkspaces :: [[Char]]
myWorkspaces = [ "1:web", "2:emacs", "3:mutt", "4", "5", "6", "7", "8", "9:jrnl" ]

myPP :: PP
myPP = def { ppTitle   = xmobarColor "#8DBF27" "" . shorten myTitleLength
           , ppCurrent = xmobarColor "#D5CD6A" "" . wrap "[" "]"
           , ppHidden  = xmobarColor "#A52A2A" "" . noScratchPad
           , ppLayout  = xmobarColor "#62C5D1" ""
           , ppSep     = " :: "
           , ppWsSep   = " "
           }
  where
    noScratchPad ws = if ws == "NSP"
                      then ""
                      else ws

myXPConfig :: XPConfig
myXPConfig = def
             { font              = "xft:Inconsolata-zi4"
             , bgColor           = "#A52A2A"
             , fgColor           = "#D5CD6A"
             , bgHLight          = "#2F2F2F"
             , fgHLight          = "#F0E0AF"
             , promptBorderWidth = 0
             , position          = Top
             , height            = 15
             , historySize       = 50
             , historyFilter     = deleteConsecutive
             }

myCalcConfig :: XPConfig
myCalcConfig = def
               { font                = "xft:Inconsolata-zi4"
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

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm              , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm              , xK_p     ), spawn "dmenu_run -p $ -nf white -sb brown -sf yellow")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

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

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Goto next workspace
    , ((modm,               xK_Right ), nextWS)

    -- Goto previous workspace
    , ((modm,               xK_Left  ), prevWS)

    -- Chromium
    , ((modm,               xK_i     ), runOrRaiseMaster "chromium" (className =? "Chromium"))

    -- Mutt
    , ((modm,               xK_y     ), raiseMaybe (runInTerm "-title mutt"  "zsh -c 'mutt'") (title =? "mutt"))

    -- Pavucontrol
    , ((modm,               xK_v     ), runOrRaiseMaster "pavucontrol" (className =? "Pavucontrol"))

    -- Volume Up
    , ((0,   xF86XK_AudioRaiseVolume ), spawn "~/bin/pa_vol_up")

    -- Volume Down
    , ((0,   xF86XK_AudioLowerVolume ), spawn "~/bin/pa_vol_down")

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

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Display items in a 2D grid and let the user select from it
    , ((modm              , xK_g     ), goToSelected def)

    -- Push the master window to the next window in the stack
    , ((modm .|. shiftMask, xK_Return), dwmpromote)

    -- Make screenshot with scrot
    , ((modm              , xK_Print ), spawn "scrot -e 'mv $f /tmp/'")

    -- Pop open a dmenu with window titles to be taken to the corresponding workspace
    , ((modm .|. shiftMask, xK_g     ), gotoMenu)

    -- Pop open a dmenu with window titles to dragged it into the current workspace
    , ((modm .|. shiftMask, xK_b     ), bringMenu)

    -- Pop open a dmenu to control MPD
    -- , ((modm              , xK_x     ), spawn "/home/dogsleg/bin/dmenu_mpd.sh")

    -- Pop open a dmenu to poweroff/reboot/suspend/lock
    , ((modm              , xK_z     ), spawn "/home/domino/bin/dmenu_shutdown.sh")

    -- Pop tiny terminal window via Scratchpad
    , ((modm              , xK_grave ), scratchpadSpawnAction def {terminal = "urxvtc -name terminal"})

    -- Shell Prompt to run a shell command
    , ((modm .|. controlMask,   xK_x ), shellPrompt myXPConfig)

    -- MPD Prompt to add album to the playlist
    -- , ((modm .|. controlMask,    xK_m), addMatching MPD.withMPD myXPConfig [MPD.Artist, MPD.Album] >> return ())

    -- Calculator Prompt to run concalc
    , ((modm              , xK_c    ), calcPrompt myCalcConfig "concalc")
    
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
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
    setRandomWallpaper ["$HOME/.wallpapers/haskell"]
    xmonad $ ewmh $ def
        { workspaces         = myWorkspaces
        , terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , focusedBorderColor = myFocusedBorderColor

        , modMask            = myModMask
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        , handleEventHook    = mconcat
                               [ perWindowKbdLayout
                               , docksEventHook
                               , handleEventHook def ]

        , manageHook = manageDocks <+> myManageHook
                        <+> manageHook def
        , layoutHook = avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn xmproc }
        }
