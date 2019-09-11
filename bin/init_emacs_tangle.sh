#!/bin/bash
#
# File: INIT_EMACS_TANGLE
# Copyright © 2019, pedro, all rights reserved.
# Created: 10 setembro 2019
#

# wrap each argument in the code required to call tangle on it
DIR="$(pwd)/files/"
FILE="init-emacs.org"

echo "============"
echo "Tangle"
echo "============"
echo "Dir : $DIR"
echo "File: $FILE"
echo ""
emacs -nw --batch --eval "
(progn
  (require 'org-install)
  (find-file (expand-file-name \"$FILE\" \"$DIR\"))
  (org-babel-tangle)
  (kill-buffer))"

echo "============"
echo

# init_emacs_tangle.sh ends here
