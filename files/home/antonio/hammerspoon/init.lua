hs.window.animationDuration = 0

-- what i want is a modal keymap, like I do cmd and then cmd +
-- something has a different functionality

-- full screen
hs.hotkey.bind({"cmd", "shift"}, "return", function ()
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
hs.hotkey.bind({"cmd", "shift"}, "h", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "shift"}, "j", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.h / 2
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "shift"}, "k", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "shift"}, "l", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.w / 2
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

-- move to the next windows with cmd+j, cmd+k
-- caffeine replacement
-- hotkeys to the usual apps

hs.hotkey.bind({"cmd", "shift"}, "r", function()
    hs.notify.new({title="Hammerspoon", informativeText="Config reloaded"}):send():release()
end)
