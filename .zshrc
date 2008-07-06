HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd notify
bindkey -e

zstyle ':completion:*' completer _complete _correct
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' '' 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/sadovnikov/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit; prompt walters 

alias ls='ls --color=always -F -a'
alias lss='ls --color=always -F -a | sort -dfu'
alias lsl='ls --color=always -F -a -l | sort -dfu'

alias e='gedit'
alias lp='source goto_lp.sh'
alias size='du -xsch /'

export LANG="ru_RU.CP1251"
export EDITOR=gedit
#export DISPLAY=sadovnikov:0

export MAKE_HOME=$SRCROOT/makeincl
export MTARGET=linux-2-6-x86-threaded-gcc
export SCRIPTS=~/bin
export THIRD_PARTY_ROOT=/var/alertlogic
  
# CHANGE THE PROJECT
#source bin/chproj.sh

export PERL5LIB=$BLDROOT/$MTARGET/obj/lm/stsquery/perl



export PATH=$PATH:/home/sadovnikov/template/bld/bin:$SRCROOT/third_party/mDNSResponder/mDNSPosix/build/prod
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/sadovnikov/template/bld/bin

# DYNAMIC HEADER
case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;%m: %~\a"}
        ;;
#    precmd () {print -Pn "\e]0;%n@%m: %~\a"}
esac


cd ~/hukka_project
source .rc
