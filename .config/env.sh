#
# ~/.config/env.sh
#

# Log entry
echo "$(date +%T) open  ~/.config/env.sh" >> ~/.log/rc.log


# Big history file to go with the prefix history search functionality
export HISTSIZE=500000
export HISTFILESIZE=100000
# Erase duplicate entries and ignore both dublicate entries and entries beginning with space
export HISTCONTROL="erasedups:ignoreboth"
export HISTTIMEFORMAT='%F %T - '
export HISTIGNORE='yt-dlp *:encfs *:ls *'

# Add ~/.local/bin/ to PATH
export PATH="$HOME/.local/bin:$PATH"
# Add Emacs commands to path
export PATH="$HOME/.config/emacs/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"

# export EDITOR=nvim
export EDITOR='nvr -s'
export VISUAL=$EDITOR
export SUDO_EDITOR=$EDITOR
export BROWSER=$HOME/.local/bin/browser
export TERMINAL=alacritty
export GIT_EDITOR='nvr -cc split --remote-wait'
export STARSHIP_CONFIG=~/.config/starship/starship.toml
# Format 'less'
export LESS='-Mri#8j.5'
#             |||| `- center on search matches
#             |||`--- scroll horizontally 8 columns at a time
#             ||`---- case-insensitive search unless pattern contains uppercase
#             |`----- parse color codes
#             `------ show more information in prompt

# Maim styling for screenshots
export MAIM_OPTIONS="--color=0.611,0.811,0.847 --bordersize=3.0 --select --hidecursor --nodrag"

# Supress 'less' history file
export LESSHISTFILE=/dev/null
# Disable writing python history file
export PYTHONSTARTUP=~/.config/python/pythonrc

# MAC-addresses of some BT-devices
export MAC_NURA="$(<~/.config/data/nura.mac)"
export MAC_CUBE66="$(<~/.config/data/cube66.mac)"
export MAC_BTR011="$(<~/.config/data/btr011.mac)"
export MAC_JBL="$(<~/.config/data/jbl.mac)"

# IPs
export PI_IP="$(<~/.config/data/pi.ip)"

# location
export CITY="$(<~/.config/data/location)"
export LON="$(<~/.config/data/longitude)"
export LAT="$(<~/.config/data/latitude)"

# Format sdcv dictionary output https://wiki.archlinux.org/title/Sdcv#Output_Formatting
export SDCV_PAGER='less --quit-if-one-screen -RX'

export LS_COLORS='always'

export FZF_DEFAULT_OPTS="
	--color=fg:#908caa,bg:#191724,hl:#ebbcba
	--color=fg+:#e0def4,bg+:#26233a,hl+:#ebbcba
	--color=border:#403d52,header:#31748f,gutter:#191724
	--color=spinner:#f6c177,info:#9ccfd8
	--color=pointer:#c4a7e7,marker:#eb6f92,prompt:#908caa"

# Log exit
echo "$(date +%T) close ~/.config/env.sh" >> ~/.log/rc.log

