@ECHO OFF

REM ================================================================================
REM Install Emacs onto APPDATA folder
REM   
REM When cloning the dotfiles repository the files should be copied to APPDATA folder
REM so that Emacs program can see them and use without problems
REM ================================================================================

REM Change variables to point to the correct path.
SET SUFIX=C:\Users\ehidped
SET gitDirectory=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files
SET initFile=%gitDirectory%\init-emacs.el

SET emacsDirectory=%APPDATA%\.emacs.d\

ECHO --------------------------------------------------------
ECHO Installing Emacs Configuration
ECHO Emacs Directory:
ECHO   %emacsDirectory%
ECHO Git Directory:
ECHO   %gitDirectory%
ECHO Init File:
ECHO   %initFile%

SET /p OPTION="Proceed? [y/N]: "

REM ==================================================================
REM Handle user's option
IF %OPTION%==y GOTO :install
IF %OPTION%==Y GOTO :install
GOTO :end

REM ==================================================================
REM Install
:install
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

REM ==================================================================

:end
PAUSE
