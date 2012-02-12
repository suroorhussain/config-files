zstyle :compinstall filename '/home/jvanwink/.zshrc'

autoload -Uz compinit
compinit
autoload -U colors && colors

setopt completeinword
PS1="%{$fg[green]%}%n@%m:%{$fg[cyan]%}%~%{$reset_color%}%% "

if [ -d "/opt/local/bin" ] ; then
    PATH="/opt/local/bin:$PATH"
    LIBRARY_PATH="/opt/local/include:/opt/include:$LIBRARY_PATH"
    LD_LIBRARY_PATH="/opt/local/lib:/opt/lib:$LD_LIBRARY_PATH"
    DYLD_LIBRARY_PATH="/opt/local/lib:$DYLD_LIBRARY_PATH"
fi

for new_path in \
    "/opt/local/sbin" \
    "/opt/local/libexec/gnubin" \
    "$HOME/opt/bin" \
    "$HOME/bin" \
    "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin" ; do
    if [ -d $new_path ] ; then
        PATH="$new_path:$PATH"
    fi
done

function act {
    source $HOME/virt/$1/bin/activate
}

function milomode {
    export PYTHONPATH=/pluto:/pluto/pycloud
    export MAGIC_ENV=development
}

eval "`dircolors -b`"
export EDITOR='emacsclient'
export ALTERNATE_EDITOR='emacsnw'
export VISUAL=$EDITOR
export PAGER=less

case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

export LESS='-S -R'
export GREP_OPTIONS='--color=auto'
export LS_OPTIONS="-b --color=auto"
export DEBEMAIL="justin.vanwinkle@gmail.com"
export DEBFULLNAME="Justin Van Winkle"

alias ls="ls $LS_OPTIONS"
alias gf='find | grep -v \.pyc$ | grep'
alias fa='find | ack-grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias ack=ack-grep

function pssh {
    machines=`~/repos/ops-tools-misc/bin/findpool -s $1`
    RETVAL=$?
    if [ $RETVAL -eq 0 ]; then
        cssh -ljvanwinkle $machines
    fi
}

function set_python_pwd {
    export PYTHONPATH=`pwd`
}

