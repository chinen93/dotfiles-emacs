@ECHO OFF

REM ================================================================================
REM Script to tangle emacs org mode files into elisp
REM 
REM Emacs org files are not read on startup only elisp ones.
REM So if this script is not run, the changes on emacs config org files are not
REM shown on.
REM ================================================================================

IF NOT DEFINED gitFolder (
    echo Variable gitFolder is not defined.
    goto :end
)
IF NOT DEFINED emacsProgramFolder (
    echo Variable emacsProgramFolder is not defined.
    goto :end
)

REM Windows uses backslash to separate directories change them to forward slashes
SET emacsFilesFolderForwardSlash=%emacsFilesFolder:\=/%
SET emacsInitialOrg=%emacsFilesFolderForwardSlash%/init-emacs.org

ECHO --------------------------------------------------------
ECHO Tangle Emacs files
ECHO Emacs path: %emacsProgram%
ECHO Emacs files source path: %emacsFilesFolder%
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
