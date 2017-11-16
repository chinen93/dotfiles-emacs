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

    # log message: header
    echo "##################################"
    echo "$filename"
    
    # file is not a directory: backup it
    if [ ! -d $filename ]; then 

	# exist file in $HOME: backup it in a dotfiles_old directory
        if [ -f $HOME/$filename ];then
	    mv $HOME/$filename $dirAtual/dotfiles-emacs_old

	    # log message: backup
	    echo "Backup de $HOME em $dirAtual/dotfiles_old"
        fi

	# file is a link: remove link
        if [ -L $HOME/$filename ];then
	    rm $HOME/$filename

	    # log message: remove link
	    echo "Removendo link $HOME/$filename"
        fi
    fi

    # If filename is emacs, put an '.' in front of it
    if [ $filename = "emacs" ]; then
	ln -s $dirFiles/$filename $HOME/.$filename
    else
	#  else make link between file in $HOME and the one in git
	ln -s $dirFiles/$filename $HOME/$filename
    fi

    # log message: link file
    echo "Linkando de $dirFiles para $HOME"

    #log message: footer
    echo ''
done;
echo ""

# go back to atual directory
cd $dirAtual
