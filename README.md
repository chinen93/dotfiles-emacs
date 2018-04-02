# Dotfiles-Emacs

Emacs Configuration for my personal use. The `C-x` keystroke was giving
me the pinky problem so I create an alternative for it `M-v` without
removing the `C-x` neither the `M-x`.

# How to Install

Clone the repository / fork it /  or just download it:

`git clone https://github.com/chinen93/dotfiles-emacs.git`

## Linux 

Execute the **install.sh**

`./install.sh`

What this shell script does is create a soft link between your home
directory and this configurations directory.

## Windows

`install_windows.bat`

What this shell script does is copy from this configuration
directory and the .emacs.d folder in your windows computer.

`update_git_folder_windows.bat`

And this other script copy back your .emacs.d directory to your
git directory. Usefull to push your changes to git.

# How the files are stored
## files/emacsConfig/

All the configurations files are stored here. They have this format
**init-*PACKAGE*.el** where *PACKAGE* is the mode or package the
configurations is about.

Most of the packages are installed and configurated by
[use-package](https://github.com/jwiegley/use-package). The ones that
aren't are buildins inside emacs. Those are the ones used when
configurating the Windows environment because I don't know how to
install packages on Windows.

## files/emacsSnippets/

Snippets for [Yasnippet](https://joaotavora.github.io/yasnippet/) are
here. I didn't use the default snippets because I wanted to experience
without them first so I could really get only the ones I needed.

## files/emacs

This file is only for loading my others configurations files. It is
the first file that emacs opens. And the default file when
configurations are made using the **customize** functions.
