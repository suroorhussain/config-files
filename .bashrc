PATH="$HOME/opt/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="/opt/local/sbin:$PATH"
PATH=$PATH:~/.cabal/bin
PATH="/opt/local/libexec/gnubin:$PATH"

if [ -d "/opt/local/bin" ] ; then
    PATH="/opt/local/bin:$PATH"
    LIBRARY_PATH="/opt/local/include:/opt/include:$LIBRARY_PATH"
    LD_LIBRARY_PATH="/opt/local/lib:/opt/lib:$LD_LIBRARY_PATH"
    DYLD_LIBRARY_PATH="/opt/local/lib:$DYLD_LIBRARY_PATH"
fi

if [ -d "/pluto" ] ; then
    export PYTHONPATH=/pluto:/pluto/pycloud
    export MAGIC_ENV=development
fi

if [ -d "/opt/local/lib/postgresql90/bin" ] ; then
    PATH="/opt/local/lib/postgresql90/bin:$PATH"
    alias psql=psql90
fi

eval "`dircolors -b`"
export EDITOR='emacs -nw'
export VISUAL=$EDITOR

export HISTCONTROL=ignoreboth
shopt -s histappend
shopt -s checkwinsize

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

force_color_prompt=yes

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

export LESS='-S -R'
export GREP_OPTIONS='--color=auto'

alias gf='find | grep -v \.pyc$ | grep'
alias fa='find | ack-grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias ack=ack-grep

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

DEBEMAIL=justin.vanwinkle@gmail.com
DEBFULLNAME="Justin Van Winkle"
export DEBEMAIL DEBFULLNAME
