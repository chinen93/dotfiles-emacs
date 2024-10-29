#!/bin/bash

# Save current directories.
dirCurrent=$(pwd)

# Where configurations files are stored.
dirFiles=$(pwd)/../files

# Create function to link configuration to HOME/
remove_create_link(){
    # Args:
    #   $1: Origin Filename (directory + file)
    #   $2: Destination Filename

    echo "##################################"
    echo "Linking '$1' to $2"

    # If link already exist, remove it.
    if [ -L $2 ];then
        rm $2
        echo "Delete old link $2"
    fi

    echo "Create new link from $1 to $2"
    ln -s $1 $2
    echo
}

# Add configuration files to HOME directory
filename="emacsConfig"
origin="$dirFiles/$filename"
destination="$HOME/$filename"
remove_create_link $origin $destination

filename="emacsSnippets"
origin="$dirFiles/$filename"
destination="$HOME/$filename"
remove_create_link $origin $destination


########################################################
# Install init-emacs into ~/.emacs.d/ directory.
########################################################
# Check if .emacs.d/ exists.
if [ ! -d $HOME/.emacs.d/ ]; then

    echo "$HOME/.emacs.d/ does not exist"
    exit 0
fi

origin="$dirFiles/init-emacs.el"
destination="$HOME/.emacs.d/init"
remove_create_link $origin $destination
