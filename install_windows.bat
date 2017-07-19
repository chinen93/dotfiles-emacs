ECHO OFF
ECHO --------------------------------------------------------
ECHO Installing Emacs Configuration
SET emacsDirectory=%APPDATA%\.emacs.d\
SET gitDirectory=%USERPROFILE%\Documents\git\dotfiles-emacs\files\
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Files Folder into Position
MKLINK /H %emacsDirectory%\init.el %gitDirectory%\emacs
ECHO --------------------------------------------------------
ECHO Moving Emacs Initial File into Position
MKLINK /D %APPDATA%\.emacsConfig %gitDirectory%\emacsConfig
MKLINK /D %APPDATA%\.snippets %gitDirectory%\snippets
ECHO --------------------------------------------------------

PAUSE
