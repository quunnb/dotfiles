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
# Add custom search scripts to PATH for dmenu
export PATH="$HOME/.local/bin/search:$PATH"
export XDG_CACHE_HOME="$HOME/.cache"

export EDITOR=nvim
export VISUAL=$EDITOR
export SUDO_EDITOR=$EDITOR
export BROWSER=$HOME/.local/bin/browser
export TERMINAL=alacritty
# Format 'less'
export LESS='-Mri#8j.5'
#             |||| `- center on search matches
#             |||`--- scroll horizontally 8 columns at a time
#             ||`---- case-insensitive search unless pattern contains uppercase
#             |`----- parse color codes
#             `------ show more information in prompt

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

# nnn
BLK="0B" CHR="0B" DIR="05" EXE="06" REG="00" HARDLINK="06" SYMLINK="06" MISSING="00" ORPHAN="09" FIFO="06" SOCK="0B" OTHER="06"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

export NNN_FIFO='/tmp/nnn.fifo'
export NNN_TMPFILE='/tmp/.lastd'
export NNN_TERMINAL='/usr/bin/alacritty'

# Log exit
echo "$(date +%T) close ~/.config/env.sh" >> ~/.log/rc.log

