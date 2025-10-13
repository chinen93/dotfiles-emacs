@ECHO OFF

REM ================================================================================
REM Script to configure basic folders for my emacs setup
REM 
REM ================================================================================

REM Change these configurations for each new installation
SET gitFolder=D:\Pedro\git
SET emacsProgramFolder=C:\Program Files\Emacs\emacs-30.2\bin

REM Windows base configurations
SET emacsBaseFolder=%APPDATA%
SET emacsProgram=%emacsProgramFolder%\runemacs.exe

REM Emacs basic configurations
SET emacsFolder=%emacsBaseFolder%\.emacs.d\

REM Git files
SET emacsFilesFolder=%gitFolder%\dotfiles-emacs\files
SET initFile=%emacsFilesFolder%\init-emacs.el
SET emacsConfigFolder=%emacsBaseFolder%\emacsConfig
SET emacsSnippetsFolder=%emacsBaseFolder%\emacsSnippets

