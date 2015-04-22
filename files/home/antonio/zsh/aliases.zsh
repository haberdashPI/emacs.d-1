alias ls='ls --color'
alias t='tmux -2'

if which hub >/dev/null; then
  alias g='hub'
else
  alias g='git'
fi

alias ccat='src-hilite-lesspipe.sh'
alias b='bundle install --binstubs'
alias v='vim'
alias diff='colordiff'
alias ap='ansible-playbook'
alias apl='ansible-playbook -l localhost'
alias ag='ag --pager less'
