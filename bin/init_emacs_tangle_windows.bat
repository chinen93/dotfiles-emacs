ECHO OFF

REM ================================================================================
REM Change the lines below to the correct paths
SET SUFIX=C:\Users\ehidped
SET emacsProgram=%SUFIX%\Desktop\Pedro\Programas\emacs-27.2-x86_64\bin\runemacs.exe
SET emacsFilesFolderOriginal=%SUFIX%\Desktop\Pedro\git\dotfiles-emacs\files

REM ================================================================================
REM Windows uses backslash to separate directories change them to forward slashes
REM 
SET emacsFilesFolderForwardSlash=%emacsFilesFolderOriginal:\=/%
SET emacsInitialOrg=%emacsFilesFolderForwardSlash%/init-emacs.org
ECHO --------------------------------------------------------

ECHO Emacs path: %emacsProgram%
ECHO Emacs files source path: %emacsFilesFolderOriginal%
ECHO Emacs files target path: %emacsInitialOrg%

"%emacsProgram%" -q -nw --eval "(progn (require 'org)(org-babel-tangle-file \"%emacsInitialOrg%\" ))"

ECHO Initial file tangled to an .el file

PAUSE
