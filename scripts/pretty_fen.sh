#!/bin/bash

~/.local/bin/pretty_fen "$(xclip -o -selection clipboard)" | xclip -selection clipboard

