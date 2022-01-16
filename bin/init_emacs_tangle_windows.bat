ECHO OFF

REM ================================================================================
REM Change the lines below to the correct paths
SET emacsProgram=%USERPROFILE%\Desktop\Pedro\emacs-27.2-x86_64\bin\runemacs.exe
SET emacsFilesFolderOriginal=%USERPROFILE%\Desktop\Pedro\git\dotfiles-emacs\files

REM ================================================================================
REM Windows uses backslash to separate directories change them to forward slashes
REM 
SET emacsFilesFolderForwardSlash=%emacsFilesFolderOriginal:\=/%
SET emacsInitialOrg=%emacsFilesFolderForwardSlash%/init-emacs.org
ECHO --------------------------------------------------------

%emacsProgram% -q -nw --eval "(progn (require 'org)(org-babel-tangle-file \"%emacsInitialOrg%\" ))"

ECHO Initial file tangled to an .el file

PAUSE
