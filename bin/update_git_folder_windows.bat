ECHO OFF
ECHO --------------------------------------------------------
ECHO Update git folder with changes on Windows

REM ==================================================================
REM Change variables to point to the correct path.
SET SUFIX=C:\Users\ehidped
SET gitFolder=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files\
SET emacsFolder=%APPDATA%

ECHO Emacs Directory: %emacsFolder%
ECHO Git Directory: %gitFolder%

ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
XCOPY %emacsFolder%\emacsConfig %gitFolder%\emacsConfig /E /I /H
XCOPY %emacsFolder%\emacsSnippets %gitFolder%\emacsSnippets /E /I /H
ECHO --------------------------------------------------------

ECHO Git folder updated, push changes to repository

PAUSE
