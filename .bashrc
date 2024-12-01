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

PS1="\[\033[36m\]$PS1\[\033[00m\]"

# Prevent overwriting existing files with the > operator. Use >| instead to overwrite
set -o noclobber
# Ignore EOF: CTRL-D doesn't log out of shells
set -o ignoreeof
# !!<space> expands to the last run command
bind Space:magic-space
# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2> /dev/null

# Treat hyphens and underscores as same in tab-completion
bind "set completion-map-case on"
# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"
# Append '/' to symlinked directories when tab-completing (at first tab press)
bind "set mark-symlinked-directories on"

# Correct spelling errors in tab completion
shopt -s dirspell 2> /dev/null
# Correct spelling errors in cd arguments
shopt -s cdspell 2> /dev/null

PROMPT_COMMAND='history -a'

# Search history by prefix
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
# These just ensure Right and Left arrows continue to work as before
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

# Append to history file rather than overwriting
shopt -s histappend
# Attempt to save each line of a multi-line-command in the same history entry separated by semicolons
shopt -s cmdhist

# Disable sending terminal output stop/resume characters with CTRL-S and CTRL-Q
if [[ -t 0 && $- = *i* ]]; then
    stty -ixon
fi

# nnn
n ()
{
    # Block nesting of nnn in subshells
    [ "${NNNLVL:-0}" -eq 0 ] || {
        echo "nnn is already running"
        return
    }

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
    # see. To cd on quit only on ^G, remove the "export" and make sure not to
    # use a custom path, i.e. set NNN_TMPFILE *exactly* as follows:
    #      NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    # The command builtin allows one to alias nnn to n, if desired, without
    # making an infinitely recursive alias
    command nnn "$@"

    [ ! -f "$NNN_TMPFILE" ] || {
        . "$NNN_TMPFILE"
        rm -f -- "$NNN_TMPFILE" > /dev/null
    }
}

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# Log exit
echo "$(date +%T) close ~/.bashrc" >> ~/.log/rc.log

