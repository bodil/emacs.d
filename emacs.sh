#!/bin/sh
export UBUNTU_MENUPROXY=""
export PATH=~/node/bin:$PATH
exec emacs $*
