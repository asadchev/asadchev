# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples


# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# locally installed emacs
if [ -d "$HOME/emacs/bin" ] ; then
    PATH="$HOME/emacs/bin:$PATH"
    export TERMCAP="$HOME/emacs/share/emacs/<version>/etc/termcap.src"
fi

PATH="/opt/autotools/bin:$PATH"

export TERM="xterm-256color"

#if [ -n "$(gcc -v 2>&1 | grep 'Red Hat 4.1')" ]; then
#    alias gcc=gcc43
#    alias g++=g++43
#fi

alias Emacs="\emacs"
alias emacs="emacs -nw"
alias em="emacs -q"
alias concat="cat"
alias mpqc="$HOME/mpqc/bin/mpqc"

export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PYTHONSTARTUP="$HOME/.startup.py"

export PATH=$PATH:/usr/local/cuda/bin

if [ -d /opt/boost/lib ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/boost/lib
fi

# if [ -d /opt/mpich2/gnu ]; then
#     export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/mpich2/gnu/lib
#     export PATH=$PATH:/opt/mpich2/gnu/bin
# fi


export EDITOR="emacs -q"

# if [ -d $HOME/opt/gcc ]; then
#     export PATH=$HOME/opt/gcc/bin:$PATH
#     export LD_LIBRARY_PATH=$HOME/opt/gcc/lib64:$LD_LIBRARY_PATH
# fi

if [ -d /usr/local/cuda/bin ]; then
    export PATH="/usr/local/cuda/bin:$PATH"
fi

if [ -f /opt/intel/bin/compilervars.sh ]; then
    source /opt/intel/bin/compilervars.sh intel64
fi

if [ -d /opt/java ]; then
    export PATH=/opt/java/bin:$PATH
    export JAVA_HOME=/opt/java
fi

if [ -x /opt/eclipse/eclipse ]; then
    alias eclipse=/opt/eclipse/eclipse
fi

if [ -f ~/.profile.cluster ]; then
    source ~/.profile.cluster
fi

