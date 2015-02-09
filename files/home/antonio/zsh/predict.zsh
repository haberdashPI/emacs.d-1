#!/bin/zsh

autoload predict-on
predict-on

antonio-accept-line () {
  zle .kill-line
  zle .accept-line
}
zle -N antonio-accept-line

antonio-history-incremental-accept-line () {
  zle .accept-line
  bindkey '^M' antonio-accept-line
}
zle -N antonio-history-incremental-accept-line

antonio-history-incremental-search-backward () {
  bindkey '^M' antonio-history-incremental-accept-line
  zle .history-incremental-search-backward
}
zle -N antonio-history-incremental-search-backward

antonio-history-incremental-search-forward () {
  bindkey '^M' antonio-history-incremental-accept-line
  zle .history-incremental-search-forward
}
zle -N antonio-history-incremental-search-forward

bindkey '^M' antonio-accept-line
bindkey '^[^M' .accept-line
bindkey '^R' antonio-history-incremental-search-backward
bindkey '^S' antonio-history-incremental-search-forward
bindkey '^X^Z' predict-on
bindkey '^Z' predict-off
