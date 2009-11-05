# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/opt/bin" ] ; then
    PATH="$HOME/opt/bin:$PATH"
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi 


# add macports path, if it exists
if [ -d "/opt/local/sbin" ] ; then
    PATH="/opt/local/sbin:$PATH"
fi

if [ -d "/opt/local/bin" ] ; then
    PATH="/opt/local/bin:$PATH"
fi

# add macports path, if it exists
if [ -d "/opt/local/lib/postgresql84/bin/" ] ; then
    PATH="/opt/local/lib/postgresql84/bin/:$PATH"
fi
