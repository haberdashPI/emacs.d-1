# prompts and themes
autoload -Uz promptinit
setopt promptsubst
promptinit

# colors
autoload -U colors && colors

# completion system
autoload -U compinit && compinit

# zmv
autoload -U zmv

# directories
setopt autocd
export CDPATH=~/code:~/Projects

# run-help
unalias run-help
autoload -U run-help
export HELPDIR=~/.zsh/help

set -o emacs

# history
export HISTFILE=$HOME/.zhistory
export SAVEHIST=2000
export HISTSIZE=2000
setopt extended_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt inc_append_history
setopt share_history

# globbing
setopt extended_glob

# ux
export WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
setopt no_flowcontrol

source ~/.zsh/aliases.zsh
source ~/.zsh/keys.zsh
source ~/.zsh/rbenv.zsh
source ~/.zsh/git.zsh
source ~/.zsh/functions.zsh

prompt antonio

[[ -n $DISPLAY ]] && [[ -z $TMUX ]] && tmux

