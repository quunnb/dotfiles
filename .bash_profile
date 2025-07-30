#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# if [ -z "$WAYLAND_DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ] ; then
#     exec niri-session
# elif [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
#   exec startx &>/dev/null
# fi
