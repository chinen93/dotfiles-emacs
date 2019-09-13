#!/bin/bash
#
# File: CLEAN
# Copyright © 2019, pedro, all rights reserved.
# Created: 11 setembro 2019
#

# If link already exist, remove it.
filename="emacsConfig"
if [ -L $HOME/$filename ];then
    rm $HOME/$filename
    echo "Delete link $HOME/$filename"
fi


# If link already exist, remove it.
filename="emacsSnippets"
if [ -L $HOME/$filename ];then
    rm $HOME/$filename
    echo "Delete link $HOME/$filename"
fi

# Check if .emacs.d/ exists.
if [ -d $HOME/.emacs.d/ ]; then

    rm $HOME/.emacs.d/init
    echo "Delete link $HOME/.emacs.d/init"
fi

# clean.sh ends here
