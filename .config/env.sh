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

# Add things to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"

export EDITOR='vim'
export VISUAL='emacsclient'
export SUDO_EDITOR=$EDITOR
export BROWSER=$HOME/.local/bin/browser
export TERMINAL=alacritty
export GIT_EDITOR='vim'
# Format 'less'
export LESS='-Mri#8j.5'
#             |||| `- center on search matches
#             |||`--- scroll horizontally 8 columns at a time
#             ||`---- case-insensitive search unless pattern contains uppercase
#             |`----- parse color codes
#             `------ show more information in prompt

# Maim styling for screenshots
export MAIM_OPTIONS="--color=1.0,0.0,0.0 --bordersize=3.0 --select --hidecursor --nodrag"

# Supress 'less' history file
export LESSHISTFILE=/dev/null
# Disable writing python history file
export PYTHONSTARTUP=~/.config/python/pythonrc

# MAC-addresses of some BT-devices
[ -f ~/.config/data/nura.mac ] && export MAC_NURA="$(<~/.config/data/nura.mac)"
[ -f ~/.config/data/cube66.mac ] && export MAC_CUBE66="$(<~/.config/data/cube66.mac)"
[ -f ~/.config/data/btr011.mac ] && export MAC_BTR011="$(<~/.config/data/btr011.mac)"
[ -f ~/.config/data/jbl.mac ] && export MAC_JBL="$(<~/.config/data/jbl.mac)"

# IPs
[ -f ~/.config/data/pi.ip ] && export PI_IP="$(<~/.config/data/pi.ip)"

# location
[ -f ~/.config/data/location ] && export CITY="$(<~/.config/data/location)"
[ -f ~/.config/data/longitude ] && export LON="$(<~/.config/data/longitude)"
[ -f ~/.config/data/latitude ] && export LAT="$(<~/.config/data/latitude)"

# Format sdcv dictionary output https://wiki.archlinux.org/title/Sdcv#Output_Formatting
export SDCV_PAGER='less --quit-if-one-screen -RX'

# Dark theme please
export GTK_THEME=0xF00BAE:dark
export GTK2_RC_FILES=$HOME/.local/share/themes/0xF00BAE/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=Adwaita-Dark

# Using highlight (http://www.andre-simon.de/doku/highlight/en/highlight.html)
export FZF_CTRL_T_OPTS="
    --walker-skip .git,node_modules,target,.go,.cache,.steam,Steam,.npm,.yarn
    --preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="
  --walker-skip .git,node_modules,target,.go,.cache,.steam,Steam,.npm,.yarn
  --preview 'tree -C {}'"

if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Log exit
echo "$(date +%T) close ~/.config/env.sh" >> ~/.log/rc.log

