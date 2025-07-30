#
# ~/.bash_aliases
#

# Log entry
echo "$(date +%T) open  ~/.bash_aliases" >> ~/.log/rc.log


alias o='xdg-open'

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd" || exit
	fi
	rm -f -- "$tmp"
}

alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ..'

# Use vim server instance
alias v='vim --servername VIMSERVER --remote-silent'
alias vim='vim --servername VIMSERVER --remote-silent'
alias vimrc='vim ~/.vim/vimrc'
alias hx='helix'
alias e='emacsclient --no-wait --reuse-frame --alternate-editor=vim'

# linter
alias lnt="golangci-lint run --config=~/.golangci.yaml ./..."

# Sudo stuff
alias N='sudo -E nnn -dH'
alias se='sudoedit'

# Open Vimwiki index
alias ww='e ~/.vimwiki/index.md'

if [ -x /usr/bin/exa ]; then
    alias l=exa
    alias ll='exa -l'
    alias ls='exa'
    alias lsd='exa --only-dirs'
    alias lsf='exa --only-files'
    alias lsg='exa --git'
else
    # Show directories with appended /
    alias ls='ls -p'
    alias ll='ls -pl'
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
alias sää='curl wttr.in/"${CITY}"?M'

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

# Bluetooth
alias btup='systemctl start bluetooth.service'
alias btdn='systemctl stop bluetooth.service'
alias btup_jbl='bluetoothctl connect "${MAC_JBL}"'
alias btup_btr011='bluetoothctl connect "${MAC_BTR011"}'
alias btup_nura='bluetoothctl connect "${MAC_NURA}"'

# Make some noise
alias whitenoise='play -n synth whitenoise'
alias pinknoise='play -n synth pinknoise'
alias brownnoise='play -n synth brownnoise'

# Terminal video
alias mpv-cli='mpv --no-config --vo=tct'

alias clock="tty-clock -Ssc"

# Dictionary
alias dic='sdcv'

alias ssh-pi='ssh dietpi@"${PI_IP}"'

alias rust-repl=evcxr
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

[ ! -x /usr/bin/neofetch ] && [ -x /usr/bin/fastfetch ] && alias neofetch='fastfetch'
alias neofetch='neofetch --source /etc/issue'

alias se='sudoedit'

# Log exit
echo "$(date +%T) close ~/.bash_aliases" >> ~/.log/rc.log

