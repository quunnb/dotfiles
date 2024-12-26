#
# ~/.bash_aliases
#

# Log entry
echo "$(date +%T) open  ~/.bash_aliases" >> ~/.log/rc.log

alias e='nvim'
alias o='xdg-open'
alias r='ranger'
alias cat='bat'

[ -x /usr/bin/nvim ] && alias vim='nvim'

# Use vim server instance
# alias v='vim --servername VIMSERVER --remote-silent'
# alias b='vim --servername VIMSERVER --remote-silent ~/buffer'
alias hx='helix'

# Sudo stuff
alias N='sudo -E nnn -dH'
alias se='sudoedit'

# Open some common rc-files
alias vimrc='e ~/.config/nvim/init.vim'
alias vimmap='e ~/.config/nvim/mappings.vim'
alias vimplug='e ~/.config/nvim/plugins.vim'
alias tridactylrc='e ~/.config/tridactyl/tridactylrc'

# Open Vimwiki index
alias ww='e ~/.vimwiki/index.md'

# Send color information even into pipes for nicer formatting
alias diff='diff --color=always' 
alias grep='grep --color=always'
alias ip='ip --color=always'
alias pacman='pacman --color=always'
alias paru='paru --color=always'

[ -x /usr/bin/rg ] && alias grep='rg'
[ -x /usr/bin/fd ] && alias find='fd'

# Show directories with appended /
alias ls='ls -p'

if [ -x /usr/bin/exa ]; then
    alias ls='exa'
    alias lsd='exa --only-dirs'
    alias lsf='exa --only-files'
    alias lsg='exa --git'
fi

# Disable wget history file at $HOME
alias wget='wget --hsts-file $HOME/.config/wget/wget-hsts'

# Dotfile git
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Exit shell
alias :q='exit'
alias :wq='exit'

# Write with zeros and delete
alias destroy='shred -zu'

# yt-dlp
alias yt='yt-dlp'
alias dl='yt-dlp'

# Download best mp4 if available
alias ytmp4='yt-dlp -f "bv*[ext=mp4]+ba[ext=m4a]/b[ext=mp4] / bv*+ba/b"'

# Download the best audio
alias yta='yt-dlp -x'
# Download best mp3
alias ytmp3='yt-dlp -x --audio-format mp3'

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

