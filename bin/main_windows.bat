@ECHO OFF

REM ==================================================================
REM Windows main menu for emacs functions
REM
REM A central location where the windows functions should be called
REM ==================================================================

:menu
ECHO --------------------------------------------------------
ECHO Windows main menu for Emacs functions
ECHO   1) Install Emacs
ECHO   2) Tangle Emacs Lisp
ECHO   3) Update git folder with changes
ECHO ==========
SET /p OPTION="Option: "

REM ==================================================================
REM Handle user's option

IF %OPTION%==1 GOTO :install_emacs
IF %OPTION%==2 GOTO :tangle_emacs
IF %OPTION%==3 GOTO :update_emacs
GOTO :menu

REM ==================================================================
REM Call real scripts here

:install_emacs
ECHO Install Emacs
CALL install_windows.bat
GOTO :end

:tangle_emacs
ECHO Tangle Emacs
CALL init_emacs_tangle_windows.bat
GOTO :end

:update_git
ECHO Update Git
CALL update_git_folder_windows.bat
GOTO :end

REM ==================================================================

:end
ECHO ==========
PAUSE


