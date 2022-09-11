ECHO OFF
ECHO --------------------------------------------------------
ECHO Installing Emacs Configuration

REM --------------------------------------------------------
REM Change variables to point to the correct path.
SET SUFIX=C:\Users\ehidped
SET gitDirectory=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files
SET initFile=%gitDirectory%\init-emacs.el

SET emacsDirectory=%APPDATA%\.emacs.d\

ECHO Emacs Directory: %emacsDirectory%
ECHO Git Directory: %gitDirectory%
ECHO Init File: %initFile%

ECHO --------------------------------------------------------
ECHO Checking if init file exists
IF NOT EXIST "%initFile%" (
   ECHO ======================================================================================
   ECHO ERROR
   ECHO ---
   ECHO %initFile% does not exist
   ECHO run "init_emacs_tangle_windows" first to create it.
   ECHO ======================================================================================
   PAUSE
   EXIT
)

ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Files Folder into Position
IF EXIST %emacsDirectory%\init.el (
   DEL %emacsDirectory%\init.el
)

COPY /Y "%initFile%" %emacsDirectory%\init.el
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
IF EXIST %APPDATA%\emacsConfig (
   RMDIR %APPDATA%\emacsConfig
)

XCOPY "%gitDirectory%\emacsConfig" %APPDATA%\emacsConfig /E /I /H

IF EXIST %APPDATA%\emacsSnippets (
   RMDIR %APPDATA%\emacsSnippets
)

XCOPY "%gitDirectory%\emacsSnippets" %APPDATA%\emacsSnippets /E /I /H

ECHO --------------------------------------------------------
ECHO Remove read-only attribute from %emacsDirectory%

ATTRIB -R %emacsDirectory%

ECHO --------------------------------------------------------
ECHO Delete "%initFile%" from installation folder
DEL "%initFile%"

ECHO --------------------------------------------------------


PAUSE
