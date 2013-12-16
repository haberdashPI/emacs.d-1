Config = {
  last_window: slate.window()
};

var relaunch = slate.operation("relaunch");
slate.bind("r:cmd,ctrl", relaunch);

focusWindow = function(title) {
  findAndFocusWindow = function() {
    var windowFound = false;
    slate.eachApp(function(app) {
      app.eachWindow(function(window) {
        if (!windowFound && window.title() === title) {
          windowFound = true;
          window.focus();
        }
      });
    });
  };
  return findAndFocusWindow;
};

fullScreen = function() {
  slate.operation("corner", { direction: "top-left"}).run();
  slate.operation("resize", { width: "+100%", height: "+100%", anchor: "top-left" }).run();
};

var focusLastWindow = function() {
  slate.operation("focus", { app: Config.last_window }).run();
};

slate.bind("1:cmd", slate.operation("focus", { app: "Google Chrome" }));
slate.bind("2:cmd", slate.operation("focus", { app: "GitHub Chat" }));
slate.bind("7:cmd", slate.operation("focus", { app: "iTunes" }));
slate.bind("8:cmd", slate.operation("focus", { app: "Adium" }));
slate.bind("9:cmd", focusWindow("Shell"));
slate.bind("esc:cmd", focusLastWindow);
slate.bind("return:cmd", fullScreen);

slate.on("appDeactivated", function(event, app) {
  Config.last_window = app.name();
});
