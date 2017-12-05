# Dotfiles-Emacs

Emacs Configuration for my personal use. The `C-x` keystroke was giving
me the pinky problem so I create an alternative for it `M-v` without
removing the `C-x` neither the `M-x`.

# How the files are stored
## files/emacsConfig/

All the configurations files are stored here. They have this format
**init-*PACKAGE*.el** where *PACKAGE* is the mode or package the
configurations is about.

## files/emacsSnippets/

Snippets for [Yasnippet](https://joaotavora.github.io/yasnippet/) are
here. I didn't use the default snippets because I wanted to experience
without them first so I could really get only the ones I needed.

## files/emacs

In this file I initialize the **package-archives** to know where to
get packages from. After that I install **use-package** if it isn't
installed yet so the other configuration files can use it. And finally
I require all my configuration files.

# How to Install
