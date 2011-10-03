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

for new_path in \
    "/opt/local/sbin" \
    "/opt/local/libexec/gnubin" \
    "$HOME/opt/bin" \
    "$HOME/bin" \
    "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin" ; do
    if [ -d $new_path ] ; then
        PATH=$new_path:$PATH
    fi
done

function act {
  source $HOME/virt/$1/bin/activate
}

eval "`dircolors -b`"
export EDITOR='emacsclient'
export ALTERNATE_EDITOR='emacsnw'
export VISUAL=$EDITOR
export ALTERNATE_EDITOR='emacs'

export HISTCONTROL=ignoreboth
shopt -s histappend
shopt -s checkwinsize

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
export LS_OPTIONS="-b --color=auto"
alias ls="ls $LS_OPTIONS"

alias gf='find | grep -v \.pyc$ | grep'
alias fa='find | ack-grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias ack=ack-grep

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
DEBEMAIL="justin.vanwinkle@gmail.com"
DEBFULLNAME="Justin Van Winkle"
export DEBEMAIL DEBFULLNAME
