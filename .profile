#
# ~/.profile
#

# Log entry
echo "$(date +%T) open  ~/.profile" |> ~/.log/rc.log

[ -f ~/.bashrc ] && . ~/.bashrc

#!/bin/sh
if [ "$TERM" = "linux" ]; then
	/bin/echo -e "
	\e]P0000000
	\e]P1EE0000
	\e]P200EE00
	\e]P3EEEE00
	\e]P40000EE
	\e]P5EE00EE
	\e]P600EEEE
	\e]P7EEEEEE
	\e]P8222222
	\e]P9FF0000
	\e]PA00FF00
	\e]PBFFFF00
	\e]PC0000FF
	\e]PDFF00FF
	\e]PE00FFFF
	\e]PFFFFFFF
	"
  clear
fi

# Log exit
echo "$(date +%T) close ~/.profile" >> ~/.log/rc.log

