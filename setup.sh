#!/bin/bash
git submodule update --init
sudo apt-get install -y clang libclang-dev nodejs npm
cd site-lisp/clang-complete-async
make
cd ../..
