#!/bin/sh

git pull

REPO_BASE="$(dirname "$PWD/$0")"
REPO_BASE="$(readlink -f $REPO_BASE)"
TRASH=$(cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1)
TRASH="/tmp/old.emacs/$TRASH"
mkdir -p $TRASH

if [ -e ~/.emacs -a ! -L ~/.emacs ]; then
    mv ~/.emacs $TRASH
fi
[ ! -L ~/.emacs ] && ln -s $REPO_BASE/.emacs ~/.emacs

[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d/

[ -d ~/.emacs.d/lisp ] || [ -h ~/.emacs.d/lisp ] && mv ~/.emacs.d/lisp $TRASH
[ ! -L ~/.emacs.d/lisp ] && ln -s $REPO_BASE/.emacs.d/lisp ~/.emacs.d/lisp

[ -d ~/.emacs.d/snippets ] || [ -h ~/.emacs.d/snippets ] && mv ~/.emacs.d/snippets $TRASH
[ ! -L ~/.emacs.d/snippets ] && ln -s $REPO_BASE/.emacs.d/snippets ~/.emacs.d/snippets

