#
# ~/.bashrc
#

# Log entry
echo "$(date +%T) open  ~/.bashrc" >> ~/.log/rc.log

[[ $- != *i*            ]] && return
[[ "$(whoami)" = "root" ]] && return
[[ -z "$FUNCNEST"       ]] && export FUNCNEST=100

[ -f ~/.config/env.sh ] && source ~/.config/env.sh
[ -f ~/.bash_aliases ] && source ~/.bash_aliases
[ -f /usr/share/doc/pkgfile/command-not-found.bash ] && source /usr/share/doc/pkgfile/command-not-found.bash

PROMPT_COMMAND='PS1_CMD1=$(__git_ps1 " (%s)")'; PS1='\n[ \u@\h ]${PS1_CMD1} \w \$ '

# Prevent overwriting existing files with the > operator. Use >| instead to overwrite
set -o noclobber
# Ignore EOF: CTRL-D doesn't log out of shells
set -o ignoreeof
# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

if [[ $- = *i* ]]; then
    # !!<space> expands to the last run command
    bind Space:magic-space
    # Treat hyphens and underscores as same in tab-completion
    bind "set completion-map-case on"
    # Display matches for ambiguous patterns at first tab press
    bind "set show-all-if-ambiguous on"
    # Append '/' to symlinked directories when tab-completing (at first tab press)
    bind "set mark-symlinked-directories on"
    # Search history by prefix
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
    # These just ensure Right and Left arrows continue to work as before
    bind '"\e[C": forward-char'
    bind '"\e[D": backward-char'
fi

# Correct spelling errors in tab completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in cd arguments
shopt -s cdspell 2> /dev/null

PROMPT_COMMAND='history -a'

# Append to history file rather than overwriting
shopt -s histappend
# Attempt to save each line of a multi-line-command in the same history entry separated by semicolons
shopt -s cmdhist

# Disable sending terminal output stop/resume characters with CTRL-S and CTRL-Q
if [[ -t 0 && $- = *i* ]]; then
    stty -ixon
fi

# Set up fzf key bindings and fuzzy completion
eval "$(fzf --bash)"

# Load colors from Xresources
if [ "$TERM" = "linux" ]; then
    _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
        echo -en "$i"
    done
    clear
fi

# Log exit
echo "$(date +%T) close ~/.bashrc" >> ~/.log/rc.log

