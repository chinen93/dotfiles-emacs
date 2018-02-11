#!/bin/bash
#
# Script to open a emacs org agenda
#

EMACS='emacs'
INITIAL_CONFIG='--no-init-file --no-window-system'
ORG='--load ~/emacsConfig/init-orgmode.el'
EVAL='--eval (my-week-and-todo-list)'

exec $EMACS $INITIAL_CONFIG $ORG $EVAL
# echo $EMACS $INITIAL_CONFIG $ORG $EVAL


# end of agenda.sh
