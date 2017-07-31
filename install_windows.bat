ECHO OFF
ECHO --------------------------------------------------------
ECHO Installing Emacs Configuration
SET emacsDirectory=%APPDATA%\.emacs.d\
SET gitDirectory=%USERPROFILE%\Documents\git\dotfiles-emacs\files\
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Files Folder into Position
IF EXIST %emacsDirectory%\init.el (
   DEL %emacsDirectory%\init.el
)
MKLINK /H %emacsDirectory%\init.el %gitDirectory%\emacs
ECHO --------------------------------------------------------
ECHO Moving Emacs Initial File into Position
IF EXIST %APPDATA%\.emacsConfig (
   RMDIR %APPDATA%\.emacsConfig
)
MKLINK /D %APPDATA%\.emacsConfig %gitDirectory%\emacsConfig

IF EXIST %APPDATA%\.snippets (
   RMDIR %APPDATA%\.snippets
)
MKLINK /D %APPDATA%\.snippets %gitDirectory%\snippets
ECHO --------------------------------------------------------

PAUSE
