@ECHO OFF

REM ================================================================================
REM Install Emacs onto APPDATA folder
REM   
REM When cloning the dotfiles repository the files should be copied to APPDATA folder
REM so that Emacs program can see them and use without problems
REM ================================================================================

IF NOT DEFINED gitFolder (
    echo Variable gitFolder is not defined.
    goto :end
)
IF NOT DEFINED emacsProgramFolder (
    echo Variable emacsProgramFolder is not defined.
    goto :end
)

ECHO --------------------------------------------------------
ECHO Installing Emacs Configuration
ECHO Emacs Directory:
ECHO   %emacsFilesFolder%
ECHO Git Directory:
ECHO   %gitFolder%
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
IF EXIST %emacsFilesFolder%\init.el (
   DEL %emacsFilesFolder%\init.el
)
COPY /Y "%initFile%" %emacsFilesFolder%\init.el

ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
IF EXIST %emacsConfigFolder% (
  RMDIR %emacsConfigFolder%
)
XCOPY "%emacsFilesFolder%\emacsConfig" %emacsConfigFolder% /E /I /H

IF EXIST %emacsSnippetsFolder% (
  RMDIR %emacsSnippetsFolder%
)
XCOPY "%emacsFilesFolder%\emacsSnippets" %emacsSnippetsFolder% /E /I /H

ECHO --------------------------------------------------------
ECHO Remove read-only attribute from %emacsFilesFolder%

ATTRIB -R %emacsFilesFolder%
PAUSE
ECHO --------------------------------------------------------
ECHO Delete "%initFile%" from installation folder
DEL "%initFile%"

ECHO --------------------------------------------------------

REM ==================================================================

:end
PAUSE
