#!/bin/bash

lichess="https://lichess.org/analysis/standard"
clipboard="$(xclip -o -selection clipboard)"
xdg-open "$lichess/$clipboard"

exit
