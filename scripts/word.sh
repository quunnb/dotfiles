#!/bin/bash

if [ $# -lt 1 ]; then
    echo "Please provide a search term"
    echo "$0 example"
    exit
elif [ $# -gt 1 ]; then
    echo "Please put multiple words in quotes"
    echo "$0 'Hello word!'"
    exit
fi

word=$1
curl dict://dict.org/d:$word

exit
