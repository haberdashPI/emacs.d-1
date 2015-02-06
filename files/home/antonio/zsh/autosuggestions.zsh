#!/bin/zsh

# Setup zsh-autosuggestions
source ~/.zsh/modules/zsh-autosuggestions/autosuggestions.zsh

# Enable autosuggestions automatically
zle-line-init() {
  zle autosuggest-start
}

zsh-autosuggestions-accept() {
  zle .end-of-line
  zle .accept-line
}

zle -N zle-line-init
zle -N zsh-autosuggestions-accept

bindkey '^[^M' zsh-autosuggestions-accept
