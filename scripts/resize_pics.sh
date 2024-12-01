#!/bin/bash

for img in $@; do
  magick convert -resize 40% "$img" "opt-$img"
done
