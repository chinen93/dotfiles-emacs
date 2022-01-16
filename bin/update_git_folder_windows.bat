ECHO OFF
ECHO --------------------------------------------------------
ECHO Update git folder with changes on Windows

REM ==================================================================
REM Change variables to point to the correct path.
SET emacsFolder=%APPDATA%
SET gitFolder=%USERPROFILE%\Desktop\Pedro\git\dotfiles-emacs\files\

ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
XCOPY %emacsFolder%\emacsConfig %gitFolder%\emacsConfig /E /I /H
XCOPY %emacsFolder%\emacsSnippets %gitFolder%\emacsSnippets /E /I /H
ECHO --------------------------------------------------------

ECHO Git folder updated, push changes to repository

PAUSE
