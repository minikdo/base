# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

export ZSH_CUSTOM=$HOME/.config/oh-my-zsh

#TERM=xterm-256color

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

if [ -f ~/.zsh_my_env ]; then
    . ~/.zsh_my_env
fi

#zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}


# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git-annex
    globalias
    man
    sudo
    virtualenvwrapper
)

# User configuration

export PATH=$HOME/bin:/usr/sbin/:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# autoload -U compinit && compinit

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# export HIST_STAMPS="yyyy-mm-dd"
export HISTSIZE=50
export SAVEHIST=0

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#

export EDITOR=e

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

alias zshrc="$EDITOR ~/.zshrc"
alias muttrc="$EDITOR ~/.muttrc"
alias ach='apt changelog'
alias jf='journalctl -f -n100'
alias uq='systemctl list-units | grep $1'
alias lf='systemctl list-units --state=failed'
alias notmuch_hooks="$EDITOR ~/.mail/.notmuch/hooks/post-new"
alias ss="ss -ltunp"
alias ip="ip -c"
alias lintian="lintian -iIEv --pedantic"
alias df="df -h --total"
alias fcp="fc -p ~/.zsh_history"
alias mc="SHELL=/bin/bash mc"
alias git-b='gbp buildpackage -us -uc --git-pristine-tar --git-export-dir=/tmp/build/build_$(date +%s) '

alias ll='ls -lah'
alias l='ls -lh'

alias -g L="| less"
alias -g M="| more"
alias -g LL="2>&1 | less"

alias m="mutt"
alias s="signal-send"
alias mcc="mc ~/.config"
alias mcl="mc ~/.local/share"
alias rs="redshift_wrapper"
alias nm="notmuch"

# avoid running ghostscript
alias gs="echo 'gs?'"
alias g="git status"

alias irssi="autossh irssi"
alias profanity="ssh profanity"

alias p="ping -w5 -c1 1.1.1.1"
alias waw="ping -w5 -c1 10.4.2.1"
alias wisla="ping -w5 -c1 10.0.0.1"

alias wp="wpa_cli"
alias mb="mbsync_wrapper minik"
# ^^^ last alias here

# My helper functions

function dq () {
    dpkg-query -W \
               -f='${db:Status-Abbrev}${binary:Package} '"$fg[blue]"'(${Version})'"$reset_color"'\n' \
        | grep -i --colour=never $1
}

function acsh () {
    apt-cache show $1 | more --exit-on-eof
}

function ipaddr () {
    echo; ip -c a; zle redisplay
}
zle -N ipaddr
bindkey "^[k" ipaddr


# Edit command line
#autoload -U edit-command-line
#zle -N edit-command-line
#bindkey '^x^e' edit-command-line

# direnv
if [ -x /usr/bin/direnv ]; then
    eval "$(direnv hook zsh)"
fi

unsetopt share_history
setopt completealiases   

apt_pref="apt"
apt_upgr="upgrade"

# fzf
if [ -f ~/.config/fzf/key-bindings.zsh ]; then
    . ~/.config/fzf/key-bindings.zsh
fi

ssh() {
  # grep -w: match command names such as "tmux-2.1" or "tmux: server"
  if ps -p $$ -o ppid= \
    | xargs -i ps -p {} -o comm= \
    | grep -qw tmux; then
    # Note: Options without parameter were hardcoded,
    # in order to distinguish an option's parameter from the destination.
    #
    #                   s/[[:space:]]*\(\( | spaces before options
    #     \(-[46AaCfGgKkMNnqsTtVvXxYy]\)\| | option without parameter
    #                     \(-[^[:space:]]* | option
    # \([[:space:]]\+[^[:space:]]*\)\?\)\) | parameter
    #                      [[:space:]]*\)* | spaces between options
    #                        [[:space:]]\+ | spaces before destination
    #                \([^-][^[:space:]]*\) | destination
    #                                   .* | command
    #                                 /\6/ | replace with destination
    tmux rename-window "$(echo "$@" \
      | sed 's/[[:space:]]*\(\(\(-[46AaCfGgKkMNnqsTtVvXxYy]\)\|\(-[^[:space:]]*\([[:space:]]\+[^[:space:]]*\)\?\)\)[[:space:]]*\)*[[:space:]]\+\([^-][^[:space:]]*\).*/\6/')"
    command ssh "$@"
    tmux set-window-option automatic-rename "on" 1> /dev/null
  else
    command ssh "$@"
  fi
}


# Prints apt history
# Usage:
#   apt-history install
#   apt-history upgrade
#   apt-history remove
#   apt-history rollback
#   apt-history list
# Based On: https://linuxcommando.blogspot.com/2008/08/how-to-show-apt-log-history.html
function apt-history() {
  case "$1" in
    install)
      zgrep --no-filename 'install ' $(ls -rt /var/log/dpkg*)
      ;;
    upgrade|remove)
      zgrep --no-filename $1 $(ls -rt /var/log/dpkg*)
      ;;
    rollback)
      zgrep --no-filename upgrade $(ls -rt /var/log/dpkg*) | \
        grep "$2" -A10000000 | \
        grep "$3" -B10000000 | \
        awk '{print $4"="$5}'
      ;;
    list)
      zgrep --no-filename '' $(ls -rt /var/log/dpkg*)
      ;;
    *)
      echo "Parameters:"
      echo " install - Lists all packages that have been installed."
      echo " upgrade - Lists all packages that have been upgraded."
      echo " remove - Lists all packages that have been removed."
      echo " rollback - Lists rollback information."
      echo " list - Lists all contains of dpkg logs."
      ;;
  esac
}


autoload -U compinit && compinit
