#!/bin/bash
git submodule update --init
sudo add-apt-repository ppa:chris-lea/node.js
sudo apt-get install -y nodejs
cd site-lisp/tern
npm install
cd ../..
