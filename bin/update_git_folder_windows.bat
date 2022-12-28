@ECHO OFF

REM ==================================================================
REM Script to update emacs's git folder
REM 
REM This is necessary, because when changing emacs configuration on Windows
REM the values are inside the APPDATA folder instead of the actual git 
REM file being versioned
REM ==================================================================

REM Change variables to point to the correct path.
SET SUFIX=C:\Users\ehidped
SET gitFolder=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files\
SET emacsFolder=%APPDATA%

ECHO --------------------------------------------------------
ECHO Update git folder with changes on Windows

ECHO Emacs Directory: %emacsFolder%
ECHO Git Directory: %gitFolder%

SET /p OPTION="Proceed? [y/N]: "

REM ==================================================================
REM Handle user's option
IF %OPTION%==y GOTO :update
IF %OPTION%==Y GOTO :update
GOTO :end

REM ==================================================================
REM Update

:update
ECHO --------------------------------------------------------
ECHO Moving Emacs Configuration Folder into Position
XCOPY %emacsFolder%\emacsConfig %gitFolder%\emacsConfig /E /I /H
XCOPY %emacsFolder%\emacsSnippets %gitFolder%\emacsSnippets /E /I /H
ECHO --------------------------------------------------------

ECHO Git folder updated, push changes to repository

REM ==================================================================

:end
PAUSE
