
import Control.Monad (liftM, liftM2, liftM3, liftM5, foldM, mapM_, msum, when)
import Data.List as L
import Data.Monoid
import Data.Traversable (traverse)
import System.Exit
import XMonad
import XMonad.Hooks.DebugKeyEvents
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName, NamedWindow)
import XMonad.Util.Themes
import XMonad.Util.EZConfig
import XMonad.Util.XUtils
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Set        as S

generalWorkspace = "g"
dashboardWorkspace = "d"

myActiveColor = "#b6ca8f"
myActiveFontColor = "white"
myInactiveColor = "#e7e7e7"
myInactiveFontColor = "black"

myTerminal      = "urxvt"
myBorderWidth   = 1
myModMask       = mod4Mask
modKeyCode      = 133
myWorkspaces    = [generalWorkspace, dashboardWorkspace]
myNormalBorderColor = myInactiveColor
myFocusedBorderColor = myActiveColor


-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Default font
myFont :: String
myFont = "xft: Inconsolata" ++
         ":pixelsize=16"
-- try without these
--         ++
--         ":antialiasing=true" ++
--         ":hinting=true" ++
--         ":autohinting=true" ++
--         ":hintstyle=hintfull" ++
--         ":rgba=rgb" ++
--         ":lcdfilter=lcdnone"

-- Bar

colorActive = xmobarColor myActiveFontColor myActiveColor
colorInactive = xmobarColor myInactiveFontColor myInactiveColor



-- max length of win titles on bar
myTitleLength = 130

-- Get a list of open windows for bar
logWindows :: Logger
logWindows = withWindowSet getWindowLog


-- getTitle :: Window -> X String
-- concatMap :: (Window -> String) -> [Window] -> String
-- msum :: [m a] -> m a

-- can we use concatMap
-- getWindowLog :: WindowSet -> X (Maybe String)
-- getWindowLog ws =
--     fmap Just $  msum . map getTitle (W.index ws)
--     where
--         curw = W.peek ws
--         getTitle w = let isCur = (Just w == curw) in
--                      fmap (formatTitle isCur . show) $ getName w
--         formatTitle f s = let s' = take myTitleLength s in
--                           let color = if f then colorActive
--                                            else colorInactive in
--                           " " ++ color s' ++ " "
--
getWindowLog :: WindowSet -> X (Maybe String)
getWindowLog ws = do
    n <- gets (currentNumWins . windowset)
    fmap Just $ foldM (addTitle (myTitleLength `div` n)) "" (W.index ws)
    where
        curw = W.peek ws
        addTitle len s w = let isCur = (Just w == curw) in
                           fmap ((s ++) . formatTitle len isCur . show) $ getName w
        formatTitle len cur s = let s' = take (len - 2) s in
                                let color = if cur then colorActive
                                                   else colorInactive in
                                " " ++ color s' ++ " "

myBar = "xmobar " ++
        "-B '" ++ myInactiveColor ++ "' " ++
        "-F '" ++ myInactiveFontColor ++ "' " ++
        "-f '" ++ myFont ++ "' " ++
        "-t '%StdinReader%}{%date%'"

layoutToString :: String -> String
layoutToString d = concatMap doXY layouts
    where
        doXY (l, s) = if l `isInfixOf` d then s else ""
        layouts = [("Tall", ".t."),
                   ("Mirror", "'"),
                   ("Float", ".f."),
                   ("Full", ". ."),
                   ("Grid", ".g.")]

myPP = xmobarPP { ppCurrent = colorActive
                , ppHidden = colorInactive
                , ppHiddenNoWindows = colorInactive
                , ppWsSep = colorInactive " "
                , ppTitle = \w -> "" -- xmobarColor myActiveFontColor myActiveColor
                , ppSep = colorInactive " "
                , ppLayout = colorInactive . layoutToString
                , ppExtras = [ logWindows ]
                }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_Tab)

-- Create and destroy workspaces

createNewWorkspace = do
    withWindowSet $ \ws -> do
        let nameWS n = let name = "w" ++ (show n) in
                       if (W.tagMember name ws)
                       then nameWS (n+1)
                       else name
        addWorkspace (nameWS 0)
        sendMessage ToggleStruts

cautiousRemoveWorkspace = do
    curtag <- gets (W.currentTag . windowset)
    when (not (elem curtag myWorkspaces)) removeEmptyWorkspace

-- Maximising

toggleMax = withFocused (\w -> windows (\ss ->
                if M.member w (W.floating ss)
                then (W.sink w ss)
                else (W.float w full ss)))
                where
                    full = W.RationalRect 0 0 1 1

modifyRect :: (Rectangle -> Rectangle) -> Window -> X ()
modifyRect rmod w = do
    disp <- asks display
    wa <- io $ getWindowAttributes disp w
    tileWindow w (rmod (Rectangle (fi $ wa_x wa)
                                  (fi $ wa_y wa)
                                  ((fi $ wa_width wa) + (fi 2*myBorderWidth))
                                  ((fi $ wa_height wa) + (fi 2*myBorderWidth))))


floatsAvoidStruts :: X ()
floatsAvoidStruts = do
        XConf { display = disp, theRoot = rootw } <- ask
        l <- gets (currentLayout . windowset)
        when ("AvoidStruts (fromList [U,D,R,L])" `isInfixOf` (show l)) $ do
            rmod <- calcGap (S.fromList [U,D,L,R]);
            withFloats $ modifyRect rmod

-- Show hide floats so we can see tiles beneath.  See also stacking and logHook.

-- doIf :: (Window -> Bool) -> (Window -> X()) -> Window -> X ()
-- and others...
doIf = liftM2 when

withWindows :: (Window -> X ()) -> X ()
withWindows f = do
    wins <- gets (W.allWindows . windowset)
    mapM_ f wins

withFloats :: (Window -> X ()) -> X ()
withFloats f = do
    floats <- gets (W.floating . windowset)
    withWindows (doIf (flip M.member floats) f)

hideFloats :: X ()
hideFloats = do
    floats <- gets (W.floating . windowset)
    let isFloat = flip M.member floats
    let getOtherWins = liftM2 (++) W.up W.down
    let doStack = flip whenJust $
            liftM2 when (not . isFloat . W.focus)
                        (mapM_ (doIf isFloat hide) . getOtherWins)
    let getScreens = liftM2 (:) W.current W.visible
    let getStack = W.stack . W.workspace
    withWindowSet $ mapM_ (doStack . getStack) . getScreens

raiseFocused :: X ()
raiseFocused = do
    disp <- asks display
    mw <- gets (W.peek . windowset)
    whenJust mw (io . (raiseWindow disp))

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
currentNumWins = length . W.index

currentlyShifting = do
    (ws, l, n, mw, floats) <- gets ((liftM5 (,,,,) W.currentTag
                                                   currentLayout
                                                   currentNumWins
                                                   W.peek
                                                   W.floating) . windowset)
    return (maybe False (\w -> M.member w floats) mw ||
            (ws == dashboardWorkspace && n > 2) ||
            ("Float" `isInfixOf` (description l)) ||
            ("Full" `isInfixOf` (description l)))

-- See also myEventHook
onModRelease = do
    XConf { display = disp, theRoot = root } <- ask
    io $ ungrabKeyboard disp currentTime
    shift <- currentlyShifting
    when shift $ windows W.shiftMaster
    return (All True)

-- Keys
--
-- Note additionalKeys after defaultConfig.  TODO: change all to EZConfig

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm, xK_y), setLayout $ Layout Full)

    , ((modm, xK_backslash), spawn $ XMonad.terminal conf)

    -- unmanage window
    -- , ((modm, xK_u), withFocused unmanage)

    -- maximise window
    , ((modm, xK_Up), toggleMax)

    -- add a tag
    , ((modm, xK_a), createNewWorkspace)

    -- delete a tag
    , ((modm, xK_d), cautiousRemoveWorkspace)

    -- launch gvim
    , ((modm, xK_g), spawn "nvim-qt")

    -- launch gimp
    , ((modm .|. shiftMask, xK_g), spawn "gimp")

    -- launch skype
    , ((modm, xK_t), spawn "skype")

     -- launch firefox
    , ((modm, xK_f), spawn "firefox")

    -- launch florence
    , ((modm .|. shiftMask, xK_k), spawn "onboard")

     -- launch coolreader
    , ((modm, xK_r), spawn "FBReader")

    -- prev tag
    , ((modm, xK_comma), prevWS)

    -- next tag
    , ((modm, xK_period), nextWS)

    -- prev screen
    , ((modm .|. mod1Mask, xK_comma), prevScreen)

    -- next screen
    , ((modm .|. mod1Mask, xK_period), nextScreen)

    -- wifi
    , ((modm, xK_w), spawn "urxvt -T netcfg -e sudo wifi-select wlan0")

    -- email
    , ((modm, xK_e), spawn "urxvt -T Mutt -name Mutt -e /home/matt/bin/start-mutt")

    -- music
    , ((modm, xK_m), spawn "urxvt -T Music -name Music -e /home/matt/bin/start-music.sh")

    -- gui music
    , ((modm .|. shiftMask, xK_m), spawn "cantata")

    -- eee music
    , ((modm .|. shiftMask, xK_e), spawn "urxvt -T Music -name Music -e /home/matt/bin/eee-do.sh")

    -- irssi
    , ((modm, xK_i), spawn "urxvt -T Irssi -name Irssi -e /home/matt/bin/start_irssi")

    -- chilon radio
    , ((modm, xK_c), spawn "urxvt -T Chilon Radio -name Music -e /home/matt/bin/chilon-mpc.sh")

    -- pause mpd
    , ((modm, xK_p), spawn "mpc -h /home/matt/.config/mpd/socket toggle")

    -- stop mpd
    , ((modm, xK_s), spawn "mpc -h /home/matt/.config/mpd/socket stop")

    -- brightness
    , ((modm, xK_equal), spawn "xbacklight -inc 10")
    , ((modm, xK_minus), spawn "xbacklight -dec 10")

    -- viking
    , ((modm, xK_v), spawn "viking /home/matt/cycling/planning.vik")

    -- viking
    , ((modm .|. shiftMask, xK_v), spawn "viking /home/matt/cycling/all.vik")

    -- brightness
    , ((modm, xK_b), spawn "gbacklight")

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
    ++

    -- mod-alt-{1,2,3}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-alt-{1,2,3}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(W.view, mod1Mask), (W.shift, shiftMask .|. mod1Mask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

button9     =  9 :: Button

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
    , ((0, button9), (\w -> nextWS))
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
myLayout =
    decoratedWorkspaces ||| Full
    where
        decoratedWorkspaces = decoration $
                              onWorkspace dashboardWorkspace (tiled ||| Mirror tiled) $
                              (tiled ||| Mirror tiled ||| grid ||| simplestFloat)

        decoration = noFrillsDeco shrinkText titleTheme

        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio   = 1/2

        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

        -- 4/3 grid
        grid = GV.Grid (4/3)

        titleTheme = defaultTheme
                        { activeColor = "#b6ca8f"
                        , activeBorderColor = "#b6ca8f"
                        , inactiveColor = "#e7e7e7"
                        , inactiveBorderColor = "#e7e7e7"
                        , fontName = myFont
                        , decoHeight = 18
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

myFullscreens = [ "Firefox"
                , "Viking"
                , "VirtualBox"
                , "Cr3"
                , "libreoffice-startcenter"
                , "libreoffice-calc"
                , "libreoffice-writer"
                , "libreoffice"
                , "LibreOffice 4.2"
                , "cantata"
                ]
myFloats = [ "mpv"
           , "Gimp"
           , "Skype"
           , "Eclipse"
           , "Dia"
           , "Hugin"
           , "Iok"
           , "Onboard"
           , "Gbacklight"
           , "Steam.exe"
           ]
myDashboardResources = ["Music", "Mutt", "Irssi"]
myKeyboard = ["Onboard", "Gbacklight"]
mySpecialWorkspaces = [dashboardWorkspace]


myManageHook = toWS <+> setFloat <+> setIgnore

-- makeFull = do
--     n <- gets (length . W.index . windowset)
--     if (n > 2) then
--         setLayout (Layout Full)
--     else return ()

-- not lifted to monad (like <&&> and <||> for log ops in composeAll/One)
-- notM :: Monad m => m Bool -> m Bool
-- notM = liftM not

setFloat = composeOne . concat $
    [ [ isFullscreen -?> doFullFloat ]
    , [ isDialog -?> doFloat ]
    , [ className =? c -?> doFloat | c <- myFloats ]
    , [ className =? c -?> doFullFloat | c <- myFullscreens ]
    ]

setIgnore = composeAll [ resource  =? "desktop_window" --> doIgnore
                       , resource  =? "kdesktop"       --> doIgnore ]


toWS = composeOne . concat $
           [ [ className =? c -?> (doF id) | c <- myKeyboard ]
           , [ resource =? t -?> doViewShift dashboardWorkspace | t <- myDashboardResources ]
           , [ fmap Just doAvoidSpecial ] ]
           where
               doViewShift = doF . viewShift
               viewShift = liftM2 (.) W.greedyView W.shift
               doAvoidSpecial = doF avoidSpecial
               avoidSpecial ss = viewShift ws ss
                                 where
                                     curws = W.currentTag ss
                                     ws = if (curws `elem` mySpecialWorkspaces)
                                          then generalWorkspace
                                          else curws




------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
---
modKeyEvents :: Event -> X All
modKeyEvents (KeyEvent {ev_event_type = t, ev_keycode = code})
  | (t == keyRelease) && (code == modKeyCode) = onModRelease
  | otherwise = return (All True)
modKeyEvents e = --do
    -- spawn ("echo " ++ (show e) ++ " >> /home/matt/log")
    return (All True)

myEventHook = modKeyEvents


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = floatsAvoidStruts <+> raiseFocused <+> hideFloats >> setWMName "LG3D"

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- hide struts by default, LG3D is a pretend window manager name to make java
-- apps work...
myStartupHook = setWMName "LG3D" <+> (broadcastMessage $ SetStruts [] [U,D,L,R])

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- this is my attempt to use ewmh to stop gimp floats spinning in a tight loop
-- with another window being tiled while still allowing iplayer to stay
-- full screen when flash loses focus, and stop de-fullscreening vimeo from
-- resizing firefox
myEwmh :: XConfig a -> XConfig a
myEwmh c = c { startupHook     = startupHook c +++ ewmhDesktopsStartup
             , handleEventHook = handleEventHook c +++ ewmhDesktopsEventHook +++ myFullscreenEventHook
             --- , logHook         = logHook c +++ ewmhDesktopsLogHook
             }
 where
    x +++ y = mappend x y
    myFullscreenEventHook :: Event -> X All
    myFullscreenEventHook e = do
        nw <- getName (ev_window e)
        if not ("Vimeo" `isInfixOf` (show nw))
        then fullscreenEventHook e
        else return (All True)



main = xmonad =<< statusBar myBar myPP toggleStrutsKey (myEwmh defaults)

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
    [ ("<XF86TouchpadToggle>", spawn "xset s off; xset -dpms")
    , ("<Print>", spawn "xset s off; xset -dpms")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 3%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 3%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioPlay>", spawn "mpc -h /home/matt/.config/mpd/socket toggle")
    , ("<XF86Forward>", spawn "mpc -h /home/matt/.config/mpd/socket next")
    , ("<XF86Back>", spawn "mpc -h /home/matt/.config/mpd/socket prev")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 15")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 15")
    , ("<XF86Launch1>", spawn "systemctl poweroff")
    , ("M#-4 <XF86PowerOff>", spawn "systemctl poweroff")
    , ("<XF86PowerOff>", spawn "easystroke send Toggle")
    , ("<XF86Search>", spawn "/home/matt/bin/connect-headphones.sh")
    ]
