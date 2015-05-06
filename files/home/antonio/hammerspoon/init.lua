-- what i want is a modal keymap, like I do cmd and then cmd +
-- something has a different functionality

-- full screen
hs.hotkey.bind({"cmd", "alt"}, "return", function ()
    local win    = hs.window.focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
    win:setFrame(f)
end)

-- move to the left/right/down/up side
-- move to the next windows with cmd+j, cmd+k
-- caffeine replacement
-- cycle between windows of the same app
-- hotkeys to the usual apps
-- focus last window (that's actually command tab)

-- reload TODO: automatic
hs.hotkey.bind({"cmd", "ctrl"}, "r", function()
    hs.notify.new({title="Hammerspoon", informativeText="Config reloaded"}):send():release()
end)