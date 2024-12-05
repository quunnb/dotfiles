#!/bin/bash

# ~/.local/bin/pretty_fen "$(xclip -o -selection clipboard)" | xclip -selection clipboard
~/.local/bin/pretty_fen "$(xsel -o --clipboard)" | xsel -i --clipboard

