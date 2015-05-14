hs.window.animationDuration = 0

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
hs.hotkey.bind({"cmd", "ctrl"}, "h", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "ctrl"}, "j", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.h / 2
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "ctrl"}, "k", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h / 2
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "ctrl"}, "l", function ()
    local win    = hs.window.focusedWindow() or hs.application.frontmostApplication():focusedWindow()
    local f      = win:frame()
    local max    = win:screen():frame()

    f.x = max.w / 2
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f)
end)

-- cmd shift j/k to go to the next/prev window

-- caffeine replacement
local caffeine = hs.menubar.new()

function updateCaffeineDisplay()
  if hs.caffeinate.get("displayIdle") then
    caffeine:setTitle("C:ON")
  else
    caffeine:setTitle("C:OFF")
  end
end

function caffeineClicked()
  hs.caffeinate.toggle("displayIdle")
  updateCaffeineDisplay()
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  hs.caffeinate.set("displayIdle", true)
  updateCaffeineDisplay()
end

-- hotkeys to the usual apps
hs.hotkey.bind({"cmd"}, "1", function()
    hs.application.launchOrFocus("Google Chrome")
end)
hs.hotkey.bind({"cmd"}, "2", function()
    hs.application.launchOrFocus("GitHub Chat")
end)
hs.hotkey.bind({"cmd"}, "3", function()
    hs.application.launchOrFocus("Textual 5")
end)
hs.hotkey.bind({"cmd"}, "4", function()
    hs.application.launchOrFocus("Slack")
end)
hs.hotkey.bind({"cmd"}, "5", function()
    hs.application.launchOrFocus("Firefox")
end)
hs.hotkey.bind({"cmd"}, "7", function()
    hs.application.launchOrFocus("iTunes")
end)
hs.hotkey.bind({"cmd"}, "9", function()
    hs.application.launchOrFocus("iTerm")
end)
hs.hotkey.bind({"cmd"}, "e", function()
    hs.appfinder.appFromName("Emacs"):mainWindow():focus()
end)

hs.hotkey.bind({"cmd", "shift"}, "r", function()
    hs.notify.new({title="Hammerspoon", informativeText="Config reloaded"}):send():release()
end)
