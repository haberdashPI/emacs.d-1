#!/bin/zsh

# Setup zsh-autosuggestions
source ~/.zsh/modules/zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically
zle-line-init() {
    zle autosuggest-start
}

zle -N zle-line-init
