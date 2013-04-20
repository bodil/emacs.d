#!/bin/bash
git submodule update --init
sudo apt-get install -y nodejs npm
cd site-lisp/tern
npm install
cd ../..
