#!/bin/bash

lichess="https://lichess.org/analysis/standard"
# clipboard="$(xclip -o -selection clipboard)"
clipboard="$(xsel -o --clipboard)"
xdg-open "$lichess/$clipboard"

exit
