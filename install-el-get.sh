#!/bin/bash

EL_GET_DIR=$HOME/.emacs.d/el-get

mkdir $EL_GET_DIR && git clone https://github.com/dimitri/el-get.git $EL_GET_DIR/el-get/
