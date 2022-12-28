@ECHO OFF

REM ================================================================================
REM Script to tangle emacs org mode files into elisp
REM 
REM Emacs org files are not read on startup only elisp ones.
REM So if this script is not run, the changes on emacs config org files are not
REM shown on.
REM ================================================================================

REM Change the lines below to the correct paths
SET SUFIX=C:\Users\ehidped
SET emacsProgram=%SUFIX%\Desktop\Pedro\Programas\emacs-27.2-x86_64\bin\runemacs.exe
SET emacsFilesFolderOriginal=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files

REM Windows uses backslash to separate directories change them to forward slashes
SET emacsFilesFolderForwardSlash=%emacsFilesFolderOriginal:\=/%
SET emacsInitialOrg=%emacsFilesFolderForwardSlash%/init-emacs.org

ECHO --------------------------------------------------------
ECHO Tangle Emacs files
ECHO Emacs path: %emacsProgram%
ECHO Emacs files source path: %emacsFilesFolderOriginal%
ECHO Emacs files target path: %emacsInitialOrg%

SET /p OPTION="Proceed? [y/N]: "

REM ==================================================================
REM Handle user's option
IF %OPTION%==y GOTO :tangle
IF %OPTION%==Y GOTO :tangle
GOTO :end

REM ==================================================================
REM Tangle
:tangle
"%emacsProgram%" -q -nw --eval "(progn (require 'org)(org-babel-tangle-file \"%emacsInitialOrg%\" ))"

ECHO Initial file tangled to an .el file

REM ==================================================================

:end
PAUSE
