Config = {
  lastAppPid: slate.app().pid()
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

var fullScreen = function() {
  slate.operation("corner", { direction: "top-left"}).run();
  slate.operation("resize", { width: "+100%", height: "+100%", anchor: "top-left" }).run();
};

var focusWindowWithPid = function(pid) {
  slate.eachApp(function(app) {
    if (app.pid() === pid) {
      app.mainWindow().focus();
    }
  });
}

var focusLastWindow = function() {
  focusWindowWithPid(Config.lastAppPid);
};

var cycleBetweenWindowsOfTheSameApp = function(appName, pids) {
  slate.eachApp(function(app) {
    if (app.name() === appName) {
      pids.push(app.pid());
    }
    pids.sort();
    slate.log(pids.length);
    if (slate.app().name() !== appName) {
      focusWindowWithPid(pids[0]);
    } else {
      currentAppPid = slate.app().pid();
      i = pids.indexOf(currentAppPid);
      if (i === (pids.length - 1)) {
        focusWindowWithPid(pids[0]);
      } else {
        focusWindowWithPid(pids[i+1]);
      }
    }
  });
};

var cycleBetweenEmacs = function() { cycleBetweenWindowsOfTheSameApp("Emacs", []); }

slate.bind("1:cmd", slate.operation("focus", { app: "Google Chrome" }));
slate.bind("2:cmd", slate.operation("focus", { app: "GitHub Chat" }));
slate.bind("7:cmd", slate.operation("focus", { app: "iTunes" }));
slate.bind("8:cmd", slate.operation("focus", { app: "Adium" }));
slate.bind("9:cmd", slate.operation("focus", { app: "iTerm" }));
slate.bind("e:cmd", cycleBetweenEmacs);
slate.bind("esc:cmd", focusLastWindow);
slate.bind("return:cmd,alt", fullScreen);

slate.on("appDeactivated", function(event, app) {
  Config.lastAppPid = app.pid();
});
