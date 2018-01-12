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

COPY /Y %gitDirectory%\emacsWindows %emacsDirectory%\init.el
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
IF EXIST %APPDATA%\emacsConfig (
   RMDIR %APPDATA%\emacsConfig
)

XCOPY %gitDirectory%\emacsConfig %APPDATA%\emacsConfig /E /I /H

IF EXIST %APPDATA%\emacsSnippets (
   RMDIR %APPDATA%\emacsSnippets
)

XCOPY %gitDirectory%\emacsSnippets %APPDATA%\emacsSnippets /E /I /H

ECHO --------------------------------------------------------
ECHO Remove read-only attribute from %emacsDirectory%

ATTRIB -R %emacsDirectory%

ECHO --------------------------------------------------------


PAUSE
