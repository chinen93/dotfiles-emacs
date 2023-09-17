@ECHO OFF

REM ==================================================================
REM Windows main menu for emacs functions
REM
REM A central location where the windows functions should be called
REM ==================================================================

:menu
ECHO --------------------------------------------------------
ECHO Windows main menu for Emacs functions
ECHO   1) Update git folder with changes
ECHO   2) Tangle Emacs Lisp
ECHO   3) Install Emacs
ECHO ==========
SET /p OPTION="Option: "

REM ==================================================================
REM Handle user's option

IF %OPTION%==1 GOTO :update_git
IF %OPTION%==2 GOTO :tangle_emacs
IF %OPTION%==3 GOTO :install_emacs
GOTO :menu

REM ==================================================================
REM Call real scripts here

:update_git
ECHO Update Git
CALL ./windows/update_git_folder_windows.bat
GOTO :end

:tangle_emacs
ECHO Tangle Emacs
CALL ./windows/init_emacs_tangle_windows.bat
GOTO :end

:install_emacs
ECHO Install Emacs
CALL ./windows/install_windows.bat
GOTO :end


REM ==================================================================

:end
ECHO ==========
PAUSE


