zstyle :compinstall filename '/home/jvanwink/.zshrc'
zstyle ':completion:*' rehash true

fpath=(/usr/local/share/zsh-completions $fpath)

source ~/.private_profile

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
setopt incappendhistory
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
    "/sbin" \
    "/usr/sbin" \
    "/opt/local/bin" \
    "/usr/local/bin" \
    "$HOME/opt/bin" \
    "$HOME/bin" \
    "/usr/local/sbin" \
    "/opt/bin" \
    "/opt/local/bin" \
    "/opt/local/sbin" \
    "/usr/local/pgsql/bin" \
    "/opt/X11/bin" \
    "/opt/local/libexec/gnubin" \
    "/opt/local/lib/postgresql94/bin" \
    "/usr/lib/postgresql/9.6/bin/" \
    "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin"; do
    if [ -d $new_path ] ; then
        PATH="$new_path:$PATH"
    fi
done

for new_man_path in \
    "/opt/X11/share/man" \
    "/opt/local/share/man"; do
    if [ -d $new_man_path ] ; then
        MANPATH="$new_man_path:$MANPATH"
    fi
done

export TZ=:/etc/localtime
export C_INCLUDE_PATH=/opt/local/include:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include
export CPLUS_INCLUDE_PATH=/opt/local/include:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include
export LD_LIBRARY_PATH=/opt/local/lib:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/lib/
export LD_INCLUDE_PATH=/opt/local/include:/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include

export GLIBCPP_FORCE_NEW=1
export GLIBCXX_FORCE_NEW=1

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
alias fa='find | ack-grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias loc='mdfind -name'
alias as="apt-cache search"
alias c-indent="gindent -nbad -bap -nbc -bbo -hnl -br -brs -c33 -cd33 -ncdb -ce -ci4 -cli0 -d0 -di1 -nfc1 -i4 -ip0 -l80 -lp -npcs -npsl -ncs -nsc -sob -nfca -cp33 -ss -ts8 -il1 -ppi 3 -brf"
alias pgnu="parallel --gnu"

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

function remssh {
    ssh-keygen -f ~/.ssh/known_hosts -R $1
}

function git-track {
    git checkout --track -b $1 origin/$1
}

function pfdiff {
    pip freeze | sort > /tmp/pip-freeze-diff
    sort requirements.txt > /tmp/pip-freeze-diff-old
    dwdiff -c /tmp/pip-freeze-diff-old /tmp/pip-freeze-diff
}

function upgrade-pip {
    pip install -U $(pip freeze | awk '{split($0, a, "=="); print a[1]}')
}

alias crunk='rsync -aHAXx --numeric-ids --delete --progress -e "ssh -T -c arcfour -o Compression=no -x"'

alias c11='clang++-mp-3.6 -std=c++11 -stdlib=libc++ -ferror-limit=2 -pedantic'

#export JAVA_HOME=`/usr/libexec/java_home`
alias frsync='rsync -avPz -e "ssh -T -c aes128-ctr -oCompression=no"'

alias jv_replace_windows_newlines="sed -i 's/^M$//'"
alias jv_strip_trailing_whitespace="sed -i -e's/[[:space:]]*$//'"
alias jv_replace_tabs_with_spaces="sed -i $'s/\t/    /g'"
alias jv_fix_trailing_newline="sed -i -e '$a\'"
alias list_ppas="grep -r --include '*.list' '^deb ' /etc/apt/ | sed -re 's/^\/etc\/apt\/sources\.list((\.d\/)?|(:)?)//' -e 's/(.*\.list):/\[\1\] /' -e 's/deb http:\/\/ppa.launchpad.net\/(.*?)\/ubuntu .*/ppa:\1/'"
