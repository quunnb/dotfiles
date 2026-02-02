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
# export HISTIGNORE='yt-dlp *:encfs *'
export HISTIGNORE='encfs *'

# Add things to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

export XDG_CACHE_HOME="$HOME/.cache"
export DOT="$HOME/dotfiles"
export VIBE_HOME="$HOME/.config/vibe"

# export EDITOR='vim --servername VIMSERVER --remote-silent'
export EDITOR='emacsclient -r -u -a emacs'
export VISUAL='emacsclient -r -u -a emacs'
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

# MAC-addresses of some devices
[ -f ~/.config/secrets/btr011.mac ] && MAC_BTR011="$(<~/.config/secrets/btr011.mac)" && export MAC_BTR011
[ -f ~/.config/secrets/jbl.mac ]    && MAC_JBL="$(<~/.config/secrets/jbl.mac)"       && export MAC_JBL
[ -f ~/.config/secrets/wlan.mac ]   && WLAN_MAC="$(<~/.config/secrets/wlan.mac)"     && export WLAN_MAC

# IPs
[ -f ~/.config/secrets/pi.ip ] && PI_IP="$(<~/.config/secrets/pi.ip)" && export PI_IP

# location
[ -f ~/.config/secrets/location ]  && CITY="$(<~/.config/secrets/location)" && export CITY
[ -f ~/.config/secrets/longitude ] && LON="$(<~/.config/secrets/longitude)" && export LON
[ -f ~/.config/secrets/latitude ]  && LAT="$(<~/.config/secrets/latitude)"  && export LAT

# Format sdcv dictionary output https://wiki.archlinux.org/title/Sdcv#Output_Formatting
export SDCV_PAGER='less --quit-if-one-screen -RX'

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

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

# LLM APIs
[ -f ~/.config/secrets/codestral.api ]  && CODESTRAL_API_KEY="$(<~/.config/secrets/codestral.api)"   && export CODESTRAL_API_KEY
[ -f ~/.config/secrets/mistral.api ]    && MISTRAL_API_KEY="$(<~/.config/secrets/mistral.api)"       && export MISTRAL_API_KEY
[ -f ~/.config/secrets/deepseek.api ]   && DEEPSEEK_API_KEY="$(<~/.config/secrets/deepseek.api)"     && export DEEPSEEK_API_KEY
[ -f ~/.config/secrets/perplexity.api ] && PERPLEXITY_API_KEY="$(<~/.config/secrets/perplexity.api)" && export PERPLEXITY_API_KEY

# Other APIs
[ -f ~/.config/secrets/tvdb.api ]  && SUBLIMINAL_REFINER_TVDB_APIKEY="$(<~/.config/secrets/tvdb.api)"               && export SUBLIMINAL_REFINER_TVDB_APIKEY
[ -f ~/.config/secrets/oscom.api ] && SUBLIMINAL_PROVIDER_OPENSUBTITLESCOM_APIKEY="$(<~/.config/secrets/oscom.api)" && export SUBLIMINAL_PROVIDER_OPENSUBTITLESCOM_APIKEY


# Log exit
echo "$(date +%T) close ~/.config/env.sh" >> ~/.log/rc.log

