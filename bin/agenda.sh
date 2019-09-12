#!/bin/bash
#
# Script to open a emacs org agenda
#

EMACS="emacs"

BASIC_FILE="$HOME/emacsConfig/elisp/init-basicConfig.el"
ORG_FILE="$HOME/emacsConfig/elisp/init-orgmode.el"
INITIAL_CONFIG="--no-init-file --no-window-system"
BASIC=" --load $BASIC_FILE"
ORG=" --load $ORG_FILE"
EVAL="--eval (my-week-and-todo-list)"

if [ -f $FILE ]; then

    exec $EMACS $INITIAL_CONFIG $BASIC $ORG $EVAL
fi


# end of agenda.sh
