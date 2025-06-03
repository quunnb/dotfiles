#
# ~/.config/env.sh
#

# Log entry
echo "$(date +%T) open ~/.config/env.sh" >> ~/.log/rc.log


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
export PATH="$HOME/.cargo/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export DOT="$HOME/dotfiles"

export EDITOR='emacsclient -t'
export VISUAL='emacsclient -a emacs'
export SUDO_EDITOR='/usr/bin/vim'
export BROWSER="$HOME/.local/bin/browser"
export TERMINAL='/usr/bin/alacritty'
export GIT_EDITOR='/usr/bin/vim'
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
[ -f ~/.config/secrets/nura.mac ] && export MAC_NURA="$(<~/.config/secrets/nura.mac)"
[ -f ~/.config/secrets/cube66.mac ] && export MAC_CUBE66="$(<~/.config/secrets/cube66.mac)"
[ -f ~/.config/secrets/btr011.mac ] && export MAC_BTR011="$(<~/.config/secrets/btr011.mac)"
[ -f ~/.config/secrets/jbl.mac ] && export MAC_JBL="$(<~/.config/secrets/jbl.mac)"

# IPs
[ -f ~/.config/secrets/pi.ip ] && export PI_IP="$(<~/.config/secrets/pi.ip)"

# location
[ -f ~/.config/secrets/location ] && export CITY="$(<~/.config/secrets/location)"
[ -f ~/.config/secrets/longitude ] && export LON="$(<~/.config/secrets/longitude)"
[ -f ~/.config/secrets/latitude ] && export LAT="$(<~/.config/secrets/latitude)"

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

# APIs
[ -f ~/.config/secrets/mistral_api_key ] && export MISTRAL_API_KEY="$(<~/.config/secrets/mistral_api_key)"

# Log exit
echo "$(date +%T) close ~/.config/env.sh" >> ~/.log/rc.log

