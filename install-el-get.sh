#!/bin/bash

EMACS_D_DIR=$HOME/.emacs.d
EL_GET_DIR=$EMACS_D_DIR/el-get

mkdir $EL_GET_DIR \
	&& git clone https://github.com/dimitri/el-get.git $EL_GET_DIR/el-get/
