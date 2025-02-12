#!/bin/bash

if [ ! $# = 1 ]; then
        echo "Usage:"
        echo "$0 <clip|save|edit>"
        exit 1;
fi

[ ! -v MAIM_OPTIONS ] && MAIM_OPTIONS="--color=0.611,0.811,0.847 --bordersize=3.0 --select --hidecursor --nodrag"

case $1 in
'clip')
        file=$(date +%s).png
        maim $MAIM_OPTIONS | tee /tmp/$file | xclip -selection clipboard -t image/png
        dunstify -r 69 -u low "Screenshot taken to clipboard"
        ;;
'save')
        filename=$(date +%s)
        path=~/Pictures/screenshots
        maim $MAIM_OPTIONS ~/Pictures/screenshots/$filename.png
        dunstify -r 69 -u low "Screenshot saved to $path/$filename.png"
        ;;
'edit')
        echo "This would open screenshot for editing" ;;
*)
        echo "parsing error"
        exit 1
        ;;
esac

canberra-gtk-play -i camera-shutter > /dev/null

exit 0
