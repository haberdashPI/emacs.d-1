#!/bin/zsh

fpath+=(~/.zsh/modules/zsh-completions/src)
source ~/.zsh/modules/prezto/modules/completion/init.zsh
setopt no_completealiases
compinit
