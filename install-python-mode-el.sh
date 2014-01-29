#!/bin/bash

SITE_LISP_DIR=~/.emacs.d/site-lisp/
PYTHON_MODE_VERSION=6.1.3
PYTHON_MODE=python-mode.el-$PYTHON_MODE_VERSION

if [[ ! -d "$SITE_LISP_DIR" ]]; then
  mkdir $SITE_LISP_DIR
fi

curl -L https://launchpad.net/python-mode/trunk/$PYTHON_MODE_VERSION/+download/$PYTHON_MODE.tar.gz | tar -xz -C $SITE_LISP_DIR
(cd $SITE_LISP_DIR && ln -s $PYTHON_MODE python-mode.el-current)
