#!/bin/bash

if [ -f ~/.emacs ];
then
	mv ~/.emacs /tmp
fi
ln -s ./.emacs ~/.emacs

if [ -d ~/.emacs.d/lisp ];
then
	mv ~/.emacs.d/lisp /tmp
fi
ln -s ./.emacs.d/lisp/ ~/.emacs.d/lisp

if [ -d ~/.emacs.d/snippets ];
then
	mv ~/.emacs.d/snippets /tmp
fi
ln -s ./.emacs.d/snippets/ ~/.emacs.d/snippets
