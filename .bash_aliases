#
# ~/.bash_aliases
#

# Log entry
echo "$(date +%T) open  ~/.bash_aliases" >> ~/.log/rc.log

# Send color information even into pipes for nicer formatting
alias diff='diff --color=always' 
alias grep='grep --color=always'
alias egrep='egrep --color=always'
alias fgrep='fgrep --color=always'
alias ip='ip --color=always'
alias pacman='pacman --color=always'
alias paru='paru --color=always'

# Don't send color codes into pipes
alias Diff='diff --color=auto' 
alias Grep='grep --color=auto'
alias Egrep='egrep --color=auto'
alias Fgrep='fgrep --color=auto'
alias Ip='ip --color=auto'
alias Pacman='pacman --color=auto'
alias Paru='paru --color=auto'

# Show directories with appended /
alias ls='ls -p'

# Use paru if it's there
[ -x /usr/bin/paru ] && alias yay='paru'

# Disable wget history file at $HOME
alias wget='wget --no-hsts'

# dotfile git
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Open some common rc-files
alias vimrc='vim ~/.vim/vimrc'
alias vimrc='nvim ~/.config/init.vim'
alias tridactylrc='vim ~/.config/tridactyl/tridactylrc'

# Open Vimwiki index
alias ww='vim ~/.vimwiki/index.md'

# Use vim server instance
# alias v='vim --servername VIMSERVER --remote-silent'
# alias b='vim --servername VIMSERVER --remote-silent ~/buffer'
# alias vim=v

alias vim='nvr'

# Helix editor
alias hx='helix'

# Exit shell
alias :q='exit'
alias :wq='exit'

# Write with zeros and delete
alias destroy='shred -zu'
alias o='xdg-open'
alias yt='yt-dlp'
alias dl='yt-dlp'

# Download the best audio
alias yta='yt-dlp -x'

# Download max size video
alias yt480='yt-dlp -f "bv*[height<=480]+ba/b[height<=480]"'
alias yt720='yt-dlp -f "bv*[height<=720]+ba/b[height<=720]"'
alias yt1080='yt-dlp -f "bv*[height<=1080]+ba/b[height<=1080]"'

alias myip='curl ipinfo.io'
alias sää="curl wttr.in/$CITY?M"

# Be verbose
alias mv='mv -v'
alias rm='rm -v'
alias cp='cp -v'
alias rmdir='rmdir -v'

alias datetime='date +"%d.%m.%Y %H:%M"'
alias nowtime='date +"%T"' # current time in 24hrs format as HH:MM:SS
alias nowdate='date +"%d-%m-%Y"' # current date in format dd-MM-YY

# Reload keyboard config
alias keyb=". ~/.xprofile"
alias N='sudo -E nnn -dH'
alias se='sudoedit'

# VPN
alias re='protonvpn-cli d; protonvpn-cli c -f'
alias pNL='protonvpn-cli c --cc NL'
alias pFI='protonvpn-cli c --cc FI'
alias pSE='protonvpn-cli c --cc SE'
alias pf='protonvpn-cli c -f'
alias pr='protonvpn-cli c -r'
alias pp2p='protonvpn-cli c --p2p'

# Bluetooth
alias btup='systemctl start bluetooth.service'
alias btdn='systemctl stop bluetooth.service'
alias btup_jbl="bluetoothctl connect $MAC_JBL"
alias btup_btr011="bluetoothctl connect $MAC_BTR011"
alias btup_nura="bluetoothctl connect $MAC_NURA"

# Make some noise
alias whitenoise='play -n synth whitenoise'
alias pinknoise='play -n synth pinknoise'
alias brownnoise='play -n synth brownnoise'

# Dictionary
alias dic='sdcv'

# Check if a program is running
alias isup='ps aux | grep'

# Project home
alias kood="cd $HOME/kood"
alias sprint="cd $HOME/kood/sprint"

alias ssh-pi="ssh dietpi@$PI_IP"

alias rust-repl=evcxr
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Log exit
echo "$(date +%T) close ~/.bash_aliases" >> ~/.log/rc.log

