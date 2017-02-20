#!/bin/bash


REPO_BASE="$(dirname "$PWD/$0/")"
TRASH=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)
TRASH="/tmp/old.emacs/$TRASH"
mkdir -p $TRASH

[ -e ~/.emacs ] && mv ~/.emacs $TRASH
ln -s $REPO_BASE/.emacs ~/.emacs

[ ! -d ~/.emacs.d ] && mkdir ~/.emacs

[ -d ~/.emacs.d/lisp ] || [ -h ~/.emacs.d/lisp ] && mv ~/.emacs.d/lisp $TRASH
ln -s $REPO_BASE/.emacs.d/lisp ~/.emacs.d/lisp

[ -d ~/.emacs.d/snippets ] || [ -h ~/.emacs.d/snippets ] && mv ~/.emacs.d/snippets $TRASH
ln -s $REPO_BASE/.emacs.d/snippets ~/.emacs.d/snippets

