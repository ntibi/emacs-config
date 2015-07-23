#!/bin/bash

PWD = pwd

if [ -f ~/.emacs ];
then
	mv ~/.emacs /tmp
fi
ln -sF ~/emacs-config/.emacs ~/.emacs

if [ -d ~/.emacs.d/lisp ];
then
	mv ~/.emacs.d/lisp /tmp
fi
ln -sF ~/emacs-config/.emacs.d/lisp/ ~/.emacs.d/lisp

if [ -d ~/.emacs.d/snippets ];
then
	mv ~/.emacs.d/snippets /tmp
fi
ln -sF ~/emacs-config/.emacs.d/snippets/ ~/.emacs.d/snippets
