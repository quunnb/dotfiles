#
# ~/.profile
#

# Log entry
echo "$(date +%T) open  ~/.profile" |> ~/.log/rc.log

[ -f ~/.bashrc ] && . ~/.bashrc

#!/bin/sh
if [ "$TERM" = "linux" ]; then
  # /bin/echo -e "
  # \e]P0191724
  # \e]P1eb6f92
  # \e]P29ccfd8
  # \e]P3f6c177
  # \e]P431748f
  # \e]P5c4a7e7
  # \e]P6ebbcba
  # \e]P7e0def4
  # \e]P826233a
  # \e]P9eb6f92
  # \e]PA9ccfd8
  # \e]PBf6c177
  # \e]PC31748f
  # \e]PDc4a7e7
  # \e]PEebbcba
  # \e]PFe0def4
  # "
	/bin/echo -e "
	\e]P0#232136
	\e]P1#eb6f92
	\e]P2#9ccfd8
	\e]P3#f6c177
	\e]P4#3e8fb0
	\e]P5#c4a7e7
	\e]P6#ea9a97
	\e]P7#e0def4
	\e]P8#393552
	\e]P9#eb6f92
	\e]PA#9ccfd8
	\e]PB#f6c177
	\e]PC#3e8fb0
	\e]PD#c4a7e7
	\e]PE#ea9a97
	\e]PF#e0def4
	"
  clear
fi

# Log exit
echo "$(date +%T) close ~/.profile" >> ~/.log/rc.log

