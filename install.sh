#!/bin/bash

TRASH=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)
TRASH="/tmp/old.emacs/$TRASH"
mkdir -p $TRASH

[ -e ~/.emacs ] && mv ~/.emacs $TRASH
ln -s ~/emacs-config/.emacs ~/.emacs

[ ! -d ~/.emacs.d ] && mkdir ~/.emacs

[ -d ~/.emacs.d/lisp ] || [ -h ~/.emacs.d/lisp ] && mv ~/.emacs.d/lisp $TRASH
ln -s ~/emacs-config/.emacs.d/lisp ~/.emacs.d/lisp

[ -d ~/.emacs.d/snippets ] || [ -h ~/.emacs.d/snippets ] && mv ~/.emacs.d/snippets $TRASH
ln -s ~/emacs-config/.emacs.d/snippets ~/.emacs.d/snippets

