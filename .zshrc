zstyle :compinstall filename '/home/jvanwink/.zshrc'
zstyle ':completion:*' rehash true

fpath=(/usr/local/share/zsh-completions $fpath)

if [ -f ~/.private_profile ] ; then
    source ~/.private_profile
fi

if [ -f ~/.zprofile ] ; then
    source ~/.zprofile
fi

autoload -Uz compinit && compinit
autoload -U colors && colors
autoload -U zmv

HISTFILE=~/.zhistory
HISTSIZE=25000
SAVEHIST=25000

setopt appendhistory
setopt completeinword
setopt extendedglob
setopt extendedhistory
setopt histexpiredupsfirst
setopt histfindnodups
setopt histignorealldups
setopt histignoredups
setopt histreduceblanks
setopt histverify
setopt incappend_history
setopt sharehistory
unsetopt banghist
unsetopt cshjunkiehistory
unsetopt histallowclobber
unsetopt histbeep
unsetopt histignorespace
unsetopt histnofunctions
unsetopt histnostore
unsetopt histsavenodups

PS1="%{$fg[green]%}%n@%m:%{$fg[cyan]%}%~%{$reset_color%}%% "

for new_path in \
    "/snap/bin" \
    "/sbin" \
    "/usr/sbin" \
    "/usr/local/bin" \
    "$HOME/opt/bin" \
    "$HOME/bin" \
    "$HOME/.cabal/bin" \
    "/usr/local/sbin" \
    "/opt/bin" \
    "/opt/local/bin" \
    "/opt/local/sbin" \
    "$HOME/.MATLAB/R2018a/bin/" \
    "/usr/lib/postgresql/11/bin/" ; do
    if [ -d $new_path ] ; then
        PATH="$new_path:$PATH"
    fi
done

if [ -n "$DESKTOP_SESSION" ]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi


# for new_man_path in \
#     "/opt/X11/share/man" \
#     "/opt/local/share/man"; do
#     if [ -d $new_man_path ] ; then
#         MANPATH="$new_man_path:$MANPATH"
#     fi
# done

#export C_INCLUDE_PATH=/opt/local/include
#export CPLUS_INCLUDE_PATH=/opt/local/include
#export LD_LIBRARY_PATH=/opt/local/lib
#export LD_INCLUDE_PATH=/opt/local/include
FG_C_PRE=$fg[green]
FG_C_PATH=$fg[cyan]
function precmd() {
    FG_C_PRE=$fg[green]
    FG_C_PATH=$fg[cyan]
    PROMPT_DISPLAY_USER=''
    DISPLAY_HOST=''

    if [[ $USER != $LOGIN_USER ]]; then
        PROMPT_DISPLAY_USER=$USER ;
    fi

    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        DISPLAY_HOST=$HOST
        FG_C_PRE=$fg[yellow]
        # many other tests omitted
    # else
    #     case $(ps -o comm= -p $PPID) in
    #         sshd|*/sshd) DISPLAY_HOST=$HOST ;;
    #     esac
    fi
}
PS1="%{$FG_C_PRE%}%{$DISPLAY_USER%}@%{$DISPLAY_HOST%}:%{$FG_C_PATH%}%~%{$reset_color%}% %(!.#.%%) "

function act {
    source $HOME/.venv/$1/bin/activate
}

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export EDITOR='emacsnw'
export ALTERNATE_EDITOR='emacsclient -t'
export VISUAL=$EDITOR
export PAGER=less
export PYTHONIOENCODING="utf_8"

export LESS='-S -R'
export LS_OPTIONS="-b --color=auto"
. ~/.ls_colors
export DEBEMAIL="justin.vanwinkle@gmail.com"
export DEBFULLNAME="Justin Van Winkle"
export TERM="xterm-256color"

alias ls="ls $LS_OPTIONS"
alias grep="grep --color=auto"
alias gf='find | grep -v \.pyc$ | grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias cat=ccat

function remssh {
    ssh-keygen -f ~/.ssh/known_hosts -R $1
}

function pfdiff {
    pip freeze | sort > /tmp/pip-freeze-diff
    sort requirements.txt > /tmp/pip-freeze-diff-old
    dwdiff -c /tmp/pip-freeze-diff-old /tmp/pip-freeze-diff
}

function upgrade-pip {
    pip install -U $(pip freeze | awk '{split($0, a, "=="); print a[1]}')
}

alias c1z='clang++ -g -Weverything -Wno-c++98-compat -pedantic -std=c++1z -march=native -ferror-limit=2'
alias c11='clang -g -Weverything -pedantic -std=c11 -march=native -ferror-limit=2'

alias frsync='rsync -avPz -e "ssh -T -c aes128-ctr -oCompression=no"'

alias jv_replace_windows_newlines="sed -i 's/^M$//'"
alias jv_strip_trailing_whitespace="sed -i -e's/[[:space:]]*$//'"
alias jv_replace_tabs_with_spaces="sed -i $'s/\t/    /g'"
alias jv_fix_trailing_newline="sed -i -e '$a\'"
alias list_ppas="grep -r --include '*.list' '^deb ' /etc/apt/ | sed -re 's/^\/etc\/apt\/sources\.list((\.d\/)?|(:)?)//' -e 's/(.*\.list):/\[\1\] /' -e 's/deb http:\/\/ppa.launchpad.net\/(.*?)\/ubuntu .*/ppa:\1/'"
