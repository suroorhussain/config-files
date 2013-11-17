zstyle :compinstall filename '/home/jvanwink/.zshrc'
fpath=(/usr/local/share/zsh-completions $fpath)

autoload -Uz compinit
compinit
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
    "/opt/local/bin" \
    "/usr/local/bin" \
    "/usr/local/opt/coreutils/libexec/gnubin" \
    "$HOME/opt/bin" \
    "$HOME/bin" \
    "/usr/local/sbin" \
    "/opt/local/lib/postgresql92/bin" ; do
    if [ -d $new_path ] ; then
        PATH="$new_path:$PATH"
    fi
done

function act {
    source $HOME/.venv/$1/bin/activate
}

eval "`dircolors -b`"
export EDITOR='emacsclient'
export ALTERNATE_EDITOR='emacsnw'
export VISUAL=$EDITOR
export PAGER=less

# case "$TERM" in
#     xterm-color) color_prompt=yes;;
# esac

export LESS='-S -R -X'
export GREP_OPTIONS='--color=auto'
export LS_OPTIONS="-b --color=auto"
export DEBEMAIL="justin.vanwinkle@gmail.com"
export DEBFULLNAME="Justin Van Winkle"
#export TERM=xterm-color

alias ls="ls $LS_OPTIONS"
alias gf='find | grep -v \.pyc$ | grep'
alias fa='find | ack-grep'
alias ggf='git ls-files | grep'
alias gg='git grep'
alias loc='mdfind -name'
alias ack=ack-grep
alias pi="port info"
alias as="apt-cache search"

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

function remssh {
    ssh-keygen -f ~/.ssh/known_hosts -R $1
}

function fetch-all {
    for repodir in ./*(/); do
        echo "fetching $repodir"
        cd $repodir;
        git fetch;
        cd ..;
    done
}

function aa_256 ()
{
    local o= i= x=`tput op` cols=`tput cols` y= oo= yy=;
    y=`printf %$(($cols-6))s`;
    yy=${y// /=};
    for i in {0..256};
    do
        o=00${i};
        oo=`echo -en "setaf ${i}\nsetab ${i}\n"|tput -S`;
        echo -e "${o:${#o}-3:3} ${oo}${yy}${x}";
    done
}

function aa_c666 ()
{
    local r= g= b= c= CR="`tput sgr0;tput init`" C="`tput op`" n="\n\n\n" t="  " s="    ";
    echo -e "${CR}${n}";
    function c666 ()
    {
        local b= g=$1 r=$2;
        for ((b=0; b<6; b++))
        do
            c=$(( 16 + ($r*36) + ($g*6) + $b ));
            echo -en "setaf ${c}\nsetab ${c}\n" | tput -S;
            echo -en "${s}";
        done
    };
    function c666b ()
    {
        local g=$1 r=;
        for ((r=0; r<6; r++))
        do
            echo -en " `c666 $g $r`${C} ";
        done
    };
    for ((g=0; g<6; g++))
    do
        c666b=`c666b $g`;
        echo -e " ${c666b}";
        echo -e " ${c666b}";
        echo -e " ${c666b}";
        echo -e " ${c666b}";
        echo -e " ${c666b}";
    done;
    #echo -e "${CR}${n}${n}"
}

export CRAM_VENV=~/.venv/cram
