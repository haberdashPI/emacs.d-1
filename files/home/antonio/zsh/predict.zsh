#!/bin/zsh

autoload predict-on
predict-on

antonio-accept-line () {
  zle .kill-line
  zle .accept-line
}

zle -N antonio-accept-line

bindkey '^M' antonio-accept-line
bindkey '^[^M' .accept-line
