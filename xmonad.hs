
import XMonad
import Data.List
import Data.Monoid
import System.Exit
import XMonad.Hooks.DebugKeyEvents
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Layout.NoFrillsDecoration 
import XMonad.Util.Themes
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import Control.Monad (liftM2, liftM3)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

generalWorkspace = "g"
dashboardWorkspace = "d"
fullWorkspace = "f"

myActiveColor = "#b6ca8f"
myActiveFontColor = "white"
myInactiveColor = "#e7e7e7"
myInactiveFontColor = "black"

myTerminal      = "urxvt"
myBorderWidth   = 1
myModMask       = mod4Mask
modKeyCode      = 133
myWorkspaces    = [generalWorkspace, dashboardWorkspace, fullWorkspace]
myNormalBorderColor = myInactiveColor
myFocusedBorderColor = myActiveColor

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Default font
myFont :: String
myFont = "xft: Liberation Mono:pixelsize=13:antialiasing=true:hinting=true:rgba=rgb:lcdfilter=lcdnone"

-- Bar
myBar = "xmobar -B '" ++ myInactiveColor ++ "' -F '" ++ myInactiveFontColor ++ "' -f '" ++ myFont ++ "' -t '%StdinReader%}{%date%'"

myPP = xmobarPP { ppCurrent = xmobarColor myActiveFontColor myActiveColor
                , ppHidden = xmobarColor myInactiveFontColor myInactiveColor
                , ppWsSep = xmobarColor "" myInactiveColor " "
                , ppTitle = xmobarColor myActiveFontColor myActiveColor
                , ppSep = xmobarColor "" myInactiveColor " "
                , ppLayout = xmobarColor myInactiveFontColor myInactiveColor
                }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_Tab)

-- Fullscreening

toggleMax = withFocused (\w -> windows (\ss ->
                if M.member w (W.floating ss)
                then (W.sink w ss)
                else (W.float w full ss)))
                where
                    full = W.RationalRect 0 0 1 1


-- Create and destroy workspaces

createNewWorkspace = do
    ws <- gets (W.workspaces . windowset)
    addWorkspace ("w" ++ (show (length ws)))

cautiousRemoveWorkspace = do
    curtag <- gets (W.currentTag . windowset)
    if not (elem curtag myWorkspaces)
    then removeEmptyWorkspace
    else return ()

-- Stacking

setModReleaseCatch :: X ()
setModReleaseCatch = do
    XConf { theRoot = root, display = disp } <- ask 
    io $ grabKeyboard disp root False grabModeAsync grabModeAsync currentTime
    return ()

changeFocus f = do
    windows f
    setModReleaseCatch

currentLayout :: W.StackSet i l a s sd -> l
currentLayout = W.layout . W.workspace . W.current

currentNumWins :: W.StackSet i l a s sd -> Int
currentNumWins ss = n
                    where
                        ms = (W.stack . W.workspace . W.current) ss
                        n = maybe 0 countWins ms
                        countWins s = 1 + length(W.up s) + length (W.down s)

currentlyShifting = do
    (ws, l, n) <- gets ((liftM3 (,,) W.currentTag currentLayout currentNumWins) . windowset)
    return ((ws == dashboardWorkspace && n > 2) || ("Float" `isInfixOf` (description l)))

-- See also myEventHook
onModRelease = do
      XConf { display = disp, theRoot = root } <- ask
      io $ ungrabKeyboard disp currentTime
      shift <- currentlyShifting
      if shift
      then windows W.shiftMaster
      else return ()
      return (All True)


-- Keys
--
-- Note additionalKeys after defaultConfig.  TODO: change all to EZConfig

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm, xK_backslash), spawn $ XMonad.terminal conf)

    -- maximise window
    , ((modm, xK_Up), toggleMax)

    -- add a tag
    , ((modm, xK_a), createNewWorkspace)
 
    -- delete a tag
    , ((modm, xK_d), cautiousRemoveWorkspace)
  
    -- launch gvim
    , ((modm, xK_g), spawn "gvim")

    -- launch skype
    , ((modm, xK_t), spawn "start-skype")

     -- launch firefox
    , ((modm, xK_f), spawn "firefox")

    -- prev tag
    , ((modm, xK_comma), prevWS)

    -- prev tag
    , ((modm, xK_period), nextWS)

    -- wifi
    , ((modm, xK_w), spawn "urxvt -T netcfg -e sudo wifi-select wlan0")

    -- email
    , ((modm, xK_e), spawn "urxvt -T Mutt -name Mutt -e /home/matt/bin/start-mutt")

    -- music
    , ((modm, xK_m), spawn "urxvt -T Music -name Music -e /home/matt/bin/start-music")

    -- irssi
    , ((modm, xK_i), spawn "urxvt -T Irssi -name Irssi -e /home/matt/bin/start_irssi")

    -- chilon radio
    , ((modm, xK_c), spawn "urxvt -T Chilon Radio -name Music -e /home/matt/bin/chilonmpc")

    -- pause mpd
    , ((modm, xK_p), spawn "mpc toggle")

    -- stop mpd
    , ((modm, xK_s), spawn "mpc stop")

    -- launch dmenu
    -- , ((modm, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p), spawn "gmrun")
 
    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm, xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)
 
    -- Move focus to the next window
    -- , ((modm, xK_Tab), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm, xK_j), changeFocus W.focusDown)
 
    -- Move focus to the previous window
    , ((modm, xK_k), changeFocus W.focusUp  )
 
    -- Move focus to the master window
    -- , ((modm, xK_m), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. mod1Mask, xK_h), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. mod1Mask, xK_l), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)
 
    -- Push window back into tiling
    -- , ((modm, xK_t), withFocused $ windows . W.sink)
    
    -- Increment the number of windows in the master area
    -- , ((modm, xK_comma), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    -- , ((modm, xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm, xK_b), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    ]
    -- From DynamicWorkspaces:
    -- mod-[1..9]       %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = onWorkspace fullWorkspace Full $ -- don't decorate full
           decoratedWorkspaces ||| Full
  where
    decoratedWorkspaces = decoration $ 
                          onWorkspace dashboardWorkspace tiled $ 
                          (tiled ||| Mirror tiled ||| simplestFloat)

    decoration = noFrillsDeco shrinkText titleTheme

    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    titleTheme = defaultTheme
                    { activeColor = "#b6ca8f"
                    , activeBorderColor = "#b6ca8f"
                    , inactiveColor = "#e7e7e7"
                    , inactiveBorderColor = "#e7e7e7"
                    , fontName = myFont
                    }
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myFullFloats = ["Firefox"]
myFloats = ["MPlayer", "Gimp"]
myDashboardResources = ["Music", "Mutt", "Irssi"]
mySpecialWorkspaces = [dashboardWorkspace, fullWorkspace]
myNoDash = ["Evince","Plugin-container"]

myManageHook = toWS <+> setFloat

setFloat = composeAll . concat $
    [ [ className =? c --> doFloat | c <- myFloats ]
    -- , [ className =? c --> doFullFloat | c <- myFullFloats ]
    , [ isFullscreen                  --> doFullFloat
      , resource  =? "desktop_window" --> doIgnore
      , resource  =? "kdesktop"       --> doIgnore ] ]


toWS = composeOne . concat $ 
           [ [ resource =? t -?> doViewShift dashboardWorkspace | t <- myDashboardResources ] 
           , [ className =? t -?> doViewShift fullWorkspace | t <- myFullFloats ]
           , [ className =? t -?> doAvoidList [dashboardWorkspace] | t <- myNoDash ]
           , [ fmap Just (doAvoidList mySpecialWorkspaces) ] ]
           where 
               doViewShift = doF . viewShift
               viewShift = liftM2 (.) W.greedyView W.shift
               doAvoidList l = doF (avoidSpecial l)
               avoidSpecial l ss = viewShift ws ss
                              where
                                  curws = W.currentTag ss
                                  ws = if (curws `elem` l)
                                       then generalWorkspace
                                       else curws




------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.

modKeyEvents :: Event -> X All
modKeyEvents (KeyEvent {ev_event_type = t, ev_keycode = code}) 
  | (t == keyRelease) && (code == modKeyCode) = onModRelease

myEventHook = modKeyEvents


------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
myLogHook = return ()
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = return ()
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- numlockMask deprecated in 0.9.1
        -- numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    } `additionalKeysP`
    [ ("<XF86TouchpadToggle>", spawn "xset s off; xset -dpms") ]
