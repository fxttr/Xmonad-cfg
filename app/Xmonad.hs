import           Data.Foldable                         ( traverse_ )
import Data.Monoid ( Endo, All )
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioLowerVolume,
      xF86XK_AudioMute,
      xF86XK_AudioNext,
      xF86XK_AudioPlay,
      xF86XK_AudioPrev,
      xF86XK_AudioRaiseVolume,
      xF86XK_AudioStop )
import System.Exit ( exitSuccess )
import           XMonad
import           XMonad.Actions.CycleWS                ( WSType(..)
                                                       , findWorkspace, anyWS
                                                       )
import           XMonad.Actions.DynamicProjects        ( Project(..)
                                                       , dynamicProjects
                                                       , switchProjectPrompt
                                                       )
import           XMonad.Actions.DynamicWorkspaces      ( removeWorkspace )
import           XMonad.Actions.RotSlaves              ( rotSlavesUp )
import           XMonad.Actions.SpawnOn                ( manageSpawn

                                                       )
import           XMonad.Actions.WithAll                ( killAll )
import           XMonad.Hooks.EwmhDesktops             ( ewmh
                                                       , ewmhDesktopsEventHook
                                                       , fullscreenEventHook
                                                       )
import           XMonad.Hooks.InsertPosition           ( Focus(Newer)
                                                       , Position(Below)
                                                       , insertPosition
                                                       )
import           XMonad.Hooks.ManageDocks              ( Direction2D(..)

                                                       , avoidStruts
                                                       , docks
                                                       , docksEventHook
                                                       )
import           XMonad.Hooks.ManageHelpers            ( (-?>)
                                                       , composeOne
                                                       , doCenterFloat
                                                       , doFullFloat
                                                       , isDialog
                                                       , isFullscreen
                                                       , isInProperty
                                                       )
import           XMonad.Hooks.UrgencyHook              ( UrgencyHook(..)
                                                       , withUrgencyHook
                                                       )
import           XMonad.Layout.Gaps                    ( gaps )
import           XMonad.Layout.MultiToggle             ( Toggle(..)
                                                       , mkToggle
                                                       , single
                                                       )
import           XMonad.Layout.MultiToggle.Instances   ( StdTransformers(NBFULL) )
import           XMonad.Layout.NoBorders               ( smartBorders )
import           XMonad.Layout.PerWorkspace            ( onWorkspace )
import           XMonad.Layout.Spacing                 ( spacing )
import           XMonad.Layout.ThreeColumns            ( ThreeCol(..) )
import           XMonad.Util.NamedScratchpad           ( NamedScratchpad(..)
                                                       , customFloating
                                                       , defaultFloating
                                                       , namedScratchpadAction
                                                       , namedScratchpadManageHook
                                                       )
import           XMonad.Util.WorkspaceCompare          ( getSortByIndex )

import qualified Data.Map                              as M
import qualified XMonad.StackSet                       as W
import qualified XMonad.Util.NamedWindows              as W
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Layout.IndependentScreens

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving Show

main :: IO ()
main = xmonad . docks . ewmh . dynProjects . urgencyHook $ def
  { terminal           = myTerminal
  , focusFollowsMouse  = True
  , clickJustFocuses   = False
  , borderWidth        = 2
  , modMask            = mod4Mask
  , keys = keybindings
  , workspaces         = withScreens 2 myWS
  , normalBorderColor  = "#BFBFBF"
  , focusedBorderColor = "#bd93f9"
  , mouseBindings      = myMouseBindings
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , startupHook        = myStartupHook
  }
 where
  dynProjects = dynamicProjects projects
  urgencyHook = withUrgencyHook LibNotifyUrgencyHook

myStartupHook :: X ()
myStartupHook = startupHook def

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- W.getName w
    maybeIdx <- W.findTag w <$> gets windowset
    traverse_ (\i -> safeSpawn "notify-send" [show name, "workspace " ++ i]) maybeIdx

myTerminal   = "urxvt"
fileManager  = "urxvt -e ranger"
appLauncher  = "dmenu_run"
screenLocker = "multilockscreen -l dim"
playerctl c  = "playerctl --player=spotify,%any " <> c

keybindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keybindings conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [ ((0, xF86XK_AudioMute              ), spawn "amixer -q set Master toggle")
    , ((0, xF86XK_AudioLowerVolume       ), spawn "amixer -q set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume       ), spawn "amixer -q set Master 5%+")
    , ((0, xF86XK_AudioPlay              ), spawn $ playerctl "play-pause")
    , ((0, xF86XK_AudioStop              ), spawn $ playerctl "stop")
    , ((0, xF86XK_AudioPrev              ), spawn $ playerctl "previous")
    , ((0, xF86XK_AudioNext              ), spawn $ playerctl "next")
    , ((modm .|. shiftMask  , xK_Return  ), spawn (XMonad.terminal conf))
    , ((modm                , xK_p       ), spawn appLauncher)
    , ((modm                , xK_x       ), runNixpkg def "Run")
    , ((modm .|. shiftMask  , xK_d       ), spawn fileManager)
    , ((modm                , xK_space   ), sendMessage NextLayout)
    , ((modm .|. shiftMask  , xK_space   ), setLayout (XMonad.layoutHook conf))
    , ((modm                , xK_f       ), sendMessage (Toggle NBFULL))
    , ((modm                , xK_o       ), switchProjectPrompt projectsTheme)
    , ((modm .|. shiftMask  , xK_q       ), io exitSuccess)
    , ((modm .|. shiftMask  , xK_c       ), kill)
    , ((modm                , xK_c       ), killAll)
    , ((modm                , xK_n       ), refresh)
    , ((modm                , xK_j       ), windows W.focusDown)
    , ((modm                , xK_k       ), windows W.focusUp)
    , ((modm                , xK_m       ), windows W.focusMaster)
    , ((modm                , xK_Return  ), windows W.swapMaster)
    , ((modm .|. shiftMask  , xK_j       ), windows W.swapDown)
    , ((modm .|. shiftMask  , xK_k       ), windows W.swapUp)
    , ((modm                , xK_h       ), sendMessage Shrink)
    , ((modm                , xK_l       ), sendMessage Expand)
    , ((modm                , xK_t       ), withFocused (windows . W.sink))
    , ((modm .|. shiftMask  , xK_Tab     ), rotSlavesUp)
    , ((modm                , xK_period  ), nextWS')
    , ((modm                , xK_comma   ), prevWS')
    , ((modm .|. shiftMask  , xK_F4      ), removeWorkspace)
    , ((modm .|. controlMask, xK_s       ), runScratchpadApp spotify)
    , ((modm .|. controlMask, xK_e       ), spawn "emacs")
    , ((modm .|. controlMask, xK_f       ), spawn "firefox")
    , ((modm .|. controlMask, xK_p       ), runScratchpadApp pavuctrl)
    ] ++ switchWsById
 where
  switchWsById =
    [ ((m .|. modm, k), windows $ onCurrentScreen f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  switchScreen =
    [ ((m .|. modm, k), screenWorkspace sc >>= flip whenJust (windows . f)) | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..], (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]]

----------- Cycle through workspaces one by one but filtering out NSP (scratchpads) -----------

nextWS' = switchWS Next
prevWS' = switchWS Prev

switchWS :: Direction1D -> X ()
switchWS dir =
  findWorkspace filterOutNSP dir anyWS 1 >>= windows . W.view

filterOutNSP :: X ([WindowSpace] -> [W.Workspace String (Layout Window) Window])
filterOutNSP =
  let g f xs = filter (\(W.Workspace t _ _) -> t /= "NSP") (f xs)
  in  g <$> getSortByIndex

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myLayout =
  avoidStruts
    . smartBorders
    . fullScreenToggle
    . sysLayout
    . cliLayout
    . msgLayout
    . devLayout
    . webLayout$ (tiled ||| Mirror tiled ||| column3 ||| full)
   where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = gapSpaced 10 $ Tall nmaster delta ratio
     full    = gapSpaced 5 Full
     column3 = gapSpaced 10 $ ThreeColMid 1 (3/100) (1/2)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     -- Gaps bewteen windows
     myGaps gap  = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
     gapSpaced g = spacing g . myGaps g

     -- Per workspace layout
     sysLayout = onWorkspace sysWs (tiled ||| full)
     devLayout = onWorkspace devWs (tiled ||| full)
     webLayout = onWorkspace webWs (tiled ||| full)
     cliLayout = onWorkspace cliWs (tiled ||| full)
     msgLayout = onWorkspace msgWs (tiled ||| full)

     -- Fullscreen
     fullScreenToggle = mkToggle (single NBFULL)

gimp      = ClassApp "Gimp"                 "gimp"
pavuctrl  = ClassApp "Pavucontrol"          "pavucontrol"
spotify   = ClassApp "Spotify"              "spotify"

myManageHook :: Query (Endo WindowSet)
myManageHook = manageApps <+> manageSpawn <+> manageScratchpads
 where
  isBrowserDialog     = isDialog <&&> className =? "Firefox"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup             = isRole =? "pop-up"
  isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isRole              = stringProperty "WM_WINDOW_ROLE"
  tileBelow           = insertPosition Below Newer
  doCalendarFloat   = customFloating (W.RationalRect (11 / 15) (1 / 48) (1 / 4) (1 / 4))
  manageScratchpads = namedScratchpadManageHook scratchpads
  anyOf :: [Query Bool] -> Query Bool
  anyOf = foldl (<||>) (pure False)
  match :: [App] -> Query Bool
  match = anyOf . fmap isInstance
  manageApps = composeOne
    [ match [ gimp ]                   -?> doFloat
    , match [ spotify ] -?> doFullFloat
    , resource =? "desktop_window"             -?> doIgnore
    , resource =? "kdesktop"                   -?> doIgnore
    , anyOf [ isBrowserDialog
            , isFileChooserDialog
            , isDialog
            , isPopup
            , isSplash
            ]                                  -?> doCenterFloat
    , isFullscreen                             -?> doFullFloat
    , pure True                                -?> tileBelow
    ]

isInstance :: App -> Query Bool
isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n

getNameCommand :: App -> (AppClassName, AppCommand)
getNameCommand (ClassApp n c) = (n, c)
getNameCommand (TitleApp n c) = (n, c)
getNameCommand (NameApp  n c) = (n, c)

getAppName :: App -> AppClassName
getAppName    = fst . getNameCommand
getAppCommand :: App -> AppCommand
getAppCommand = snd . getNameCommand

scratchpadApp :: App -> NamedScratchpad
scratchpadApp app = NS (getAppName app) (getAppCommand app) (isInstance app) defaultFloating

runScratchpadApp :: App -> X ()
runScratchpadApp = namedScratchpadAction scratchpads . getAppName

scratchpads :: [NamedScratchpad]
scratchpads = scratchpadApp <$> [ spotify, pavuctrl, gimp ]

webWs = "web"
devWs = "dev"
cliWs = "cli"
msgWs = "msg"
sysWs = "sys"

myWS :: [WorkspaceId]
myWS = [webWs, devWs, cliWs, msgWs, sysWs]

projects :: [Project]
projects =
  [ Project { projectName      = webWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = devWs
            , projectDirectory = "~/Devel"
            , projectStartHook = Nothing
            }
  , Project { projectName      = cliWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = msgWs
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = sysWs
            , projectDirectory = "/home/florian/nixos/"
            , projectStartHook = Just . spawn $ myTerminal <> " -e sudo su"
            }
  ]

projectsTheme :: XPConfig
projectsTheme = amberXPConfig
  { bgHLight = "#002b36"
  , font     = "xft:Bitstream Vera Sans Mono:size=8:antialias=true"
  , height   = 50
  , position = CenteredAt 0.5 0.5
  }

myEventHook :: Event -> X All
myEventHook = docksEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook

runNixpkg :: XPConfig -> String -> X ()
runNixpkg conf x = inputPrompt conf x ?+ \i -> spawn $ "nix-shell -p " ++ i ++ " --run " ++ i
