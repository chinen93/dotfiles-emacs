ECHO OFF
ECHO --------------------------------------------------------
ECHO Update git folder with changes on Windows

SET emacsDirectory=%APPDATA%\.emacs.d\
SET gitDirectory=%USERPROFILE%\Documents\git\dotfiles-emacs\files\

ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Files Folder into Position

COPY /Y %emacsDirectory%\init.el %gitDirectory%\emacsWindows 
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position

XCOPY %APPDATA%\emacsConfig %gitDirectory%\emacsConfig /E /I /H
XCOPY %APPDATA%\emacsSnippets %gitDirectory%\emacsSnippets /E /I /H

ECHO --------------------------------------------------------

PAUSE
