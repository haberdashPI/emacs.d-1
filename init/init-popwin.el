(require 'popwin)
(popwin-mode 1)
(setq popwin:popup-window-height 30)
(push '("*helm" :regexp t :height 30) popwin:special-display-config)