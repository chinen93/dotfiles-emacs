#!/bin/bash

# Save the directories in variables
dirAtual=$(pwd)

# where the files in git are stored 
dirFiles=$(pwd)/files

# 'dotfiles_old/' directory does not exist: create a new one
if [ ! -d $dirAtual/dotfiles-emacs_old  ];then
    mkdir $dirAtual/dotfiles-emacs_old
fi

# for each file
for filename in $(ls $dirFiles); do

    # put an '\.' in front of the filename
    file=.$filename

    # log message: header
    echo "##################################"
    echo "$file"
    
    # file is not a directory: backup it
    if [ ! -d $filename ]; then 

	# exist file in $HOME: backup it in a dotfiles_old directory
        if [ -f $HOME/$file ];then
	    mv $HOME/$file $dirAtual/dotfiles_old

	    # log message: backup
	    echo "Backup de $HOME em $dirAtual/dotfiles_old"
        fi

	# file is a link: remove link
        if [ -L $HOME/$file ];then
	    rm $HOME/$file

	    # log message: remove link
	    echo "Removendo link $HOME/$file"
        fi
    fi

    # make link between file in $HOME and the one in git
    ln -s $dirFiles/$filename $HOME/$file

    # log message: link file
    echo "Linkando de $dirFiles para $HOME"

    #log message: footer
    echo ''
done;
echo ""

# go back to atual directory
cd $dirAtual
