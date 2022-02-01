# -*- shell-script -*-
# these are my aliasses

# remote connections: alias and hosts moved to ~/.remoteconnections.sh

alias wintermkludge='echo rdesktop -K -g 1280x1024 IPADRESS -u USERNAME -d DOMAIN -n $(date +%F)'

# fun with sudo
alias please,=sudo
alias fuck='sudo $(history -p \!\!)'

alias db='mysql -u root -p XXXX'


function edicat () {
  perl -p -e 's/(?<!\?)\x27/\n/g' $*
}
function ediless {
 edicat $* | less
}

function cam-mirror {
  mplayer -fs -vf mirror -v tv:// -tv device=/dev/video0:driver=v4l2; 
}
function cam-me {
  cvlc -f v4l2:///dev/video0
}

# Variations on a Theme called ls:
alias ls='ls -hF --color=tty'
alias l='ls -l'
alias ll='ls -lA'
alias la='ll'

function lm () { 
  ls -l $* | less ; 
}
function llm () { 
  ls -lA $* | less ; 
}
#alias ltr='ls -ltr "$@" --color=always | tail -n 10'
function ltr () {
    ls -ltr "$@" --color=always | tail -n 10
}
alias lltr='ls -ltr'
alias lt='ls -lt'
alias lsr='ls -lSr'

# only dirs
alias lsd='for i in *; do if [ -d "$i" ]; then ls -d "$i"; fi; done'
alias llsd='for i in *; do if [ -d "$i" ]; then ls -dl $i; fi; done'


# view/edit/search files
alias m='less'
alias mm='less'
alias mo='more'
alias tf='tail -F'
alias mt='multitail --retry-all'

function xem () {
  xemacs $* & 
}

#function em () { 
#  emacs  $* & 
#}
alias em='emacs -nw'

#alias vi='gnuclient -nw'
#alias gc='gnuclient'
#if [ `grep --version | head -n 1|cut -d' ' -f4 | sed 's/\.//g'` -ge 251 ]; then
#  alias grep='grep --color'
#fi
alias grep='grep --color'

# Grepped
function g () {
  grep $* 2> /dev/null ; 
}
alias gi='g -i'
alias gir='g -ir'
alias gr='g -r'
alias grv='g -v'

function gm () { 
  grep $* 2> /dev/null | $PAGER ; 
}
alias gim='gm -i'
alias a='ack-grep'

# find 
function fx () {
  for i in `locate $1`; do if [ -x $i ]; then echo $i; fi; done
}

function mkddir () {
  if [ -z $1 ]; then
    mkdir $(date +%F)
    cd $(date +%F)
  else
    mkdir $(date +%F)_$1
    cd $(date +%F)_$1
  fi
}


function ff () {
    local path
    local name
    if [ $# -eq 2 ]; then
	path=${1:+ "$1"}
	name="*$2*"
    elif [ $# -eq 1 ]; then 
	path=.
	name="*$1*"
    elif [ $# -eq 0 ]; then
	path=.
	name="*"
    else 
	echo "Too many arguments to ff"
	return -1
    fi
    find $path -iname "$name" -not -path '*.git*' -not -path '*.svn*'
}
alias ffg='ff | g'

# screen
alias sw='screen -wipe'
alias s='screen'
alias sr='screen -r'
alias sls='screen -ls'

alias lo='locate'
alias loi='locate -i'

# interactive 
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mvv='mv -v'

#alias rm-rf=' rm -rf'

# directories
alias ..='cd ..'
alias ...='cd ../../'
alias md='mkdir'
alias mp='mkdir -p'
function mdd () {
  mkdir -p "$1" ; cd "$1" ; 
}

function ccd () {
  builtin cd $1 ; pushd `pwd` ; 
}

function smart-cd () {
    f=$1

    if [ -z "$f" ]; then
	cd "$HOME"
    elif [ -f "$f" ]; then 
	cd $(dirname "$f")
    else 
	cd "$f"
    fi
}

alias c=smart-cd

#alias .='pushd .'
#alias ,='popd '
#alias /='dirs'

# clear

function clrback () {
  for i in *~ *.*~ .*~ *% *.*% .*% \#*\# \#*.*\# \#.*\#; do
    test -f $i && rm $i
  done
}
function clrtex () {
  for i in *.dvi *.aux *.log *.ps *.toc *.bbl *.blg; do
    test -f $i && rm $i
  done
}


# diverse
#alias ssh='ssh -X'
alias bc='bc -l'
function = () {
    if [ -z $1 ]; then 
	genius
    else
	echo "$@" | genius
    fi
}

function logged () {
    ARGS="$@"
    date +"%Y-%m-%d %H:%M:%S  START"
    eval "$@" 2>&1 | gawk '{ print strftime("%Y-%m-%d %H:%M:%S"), $0; fflush(); }' 
    date +"%Y-%m-%d %H:%M:%S  END"
}

alias df='df -h'
alias mc='mc -x'
alias lc='wc -l'

function jwt_decode () {
    jq -R 'split(".") | .[1] | @base64d | fromjson' <<< "$1"
}

#alias hiddentop='wterm -T hidden +sb -tr -fg "#cecece" -bg "#334577" -e top&'
# alias hiddentop='urxvt -T hidden +sb -tr -fg "#9e9e9e" -bg "#336577" -geometry +796+0 -e top&'
# alias sm='/usr/sbin/sendmail -v -q'
alias ty='type'
alias dush='du -sh'
# function cmucl () {
#   rlwrap lisp $@
# }

function epoch {
  perl -MPOSIX -e "print(strftime(\"%F %T\",localtime($1)),\"\n\")"
}


# system information
alias netinfo="netstat -Watpee | sed '1d' | sed -e 's/\(Local\|Foreign\|Program\) /\1_/g'  | column -t"
alias snetinfo="sudo netinfo"

# Nice things from www.catonmat.net
alias direxporthttp='python -m SimpleHTTPServer'
function watchdir {
  local directory=$1
  if [ -z "$directory" ]; then
    directory=$(pwd)
  fi
  cd "$directory"
  watch -dn1 "ls -ldh \"$(pwd)\"; echo; df -h; echo; ls -FhlAtr | tail -n 10"
  cd - 2>&1 > /dev/null
}

alias histtop='history | awk "{a[\$2]++}END{for(i in a){print a[i] \" \" i}}" | sort -rn | head'
alias hh='history'
alias hgr='history | grep'

# The Alphabet
alias a='apropos'
alias b='bc'
# alias c='column -t'
alias c='smart-cd'
alias d='docker'
alias e='export'
alias f='file'
#alias g='grep'
alias h='head'
alias i='info'
alias j='jmacs'
alias k='konqueror `pwd`&'
#alias l='ls -l'
#alias m='less'
alias n='netstat'
alias o='more'
#alias p=''
#alias q=''
#alias r=''
#alias s=''
alias t='tmux'
#alias u=''
#alias v=''
alias w='watch -d -n 1'
#alias x=''
#alias y=''
alias z='zgrep'

# to keep things out of public version control
test -f ~/.bash_aliases_local && . ~/.bash_aliases_local
