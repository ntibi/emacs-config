;;; myconfig.el --- vanilla emacs config
;;; commentary:
;;; keybinds and modes available on default emacs >= 24
;;; code:

;; set backup dir (/tmp/emacs{uid})
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(defconst user user-real-login-name)

(menu-bar-mode -1)						; No menu bar
(setq inhibit-startup-message t)		; no startup message
(setq initial-scratch-message ";; Scratch buffer\n") ; Scratch buffer message

(setq shift-select-mode t)				; handy selection with shift

(column-number-mode t)					; print column number
(line-number-mode t)					; print line number

(set-default 'truncate-lines t)			; truncate long lines with a $

(setq scroll-margin 10)					; pre scroll
(setq scroll-step 1)					; smooth scroll (1 line by 1)
(setq scroll-conservatively 1000)		; keep prescrolling ?
(setq hscroll-margin 20)				; pre hscroll
(setq hscroll-step 1)					; smooth hscroll (1 column by 1)

(global-auto-revert-mode t)				; auto update changed files

(electric-pair-mode)

;; (add-hook 'c-mode-common-hook ')

(add-hook 'c-mode-hook '(lambda () "" (interactive) (c-toggle-auto-newline t)))
(add-hook 'c++-mode-hook (lambda () "" (interactive) (c-toggle-auto-newline t)))

(add-hook 'prog-mode-hook 'set-mode-line) ; set my mode line in the new programming buffers

(add-hook 'python-mode-hook
		  (function (lambda ()
                      (setq indent-tabs-mode t
                            tab-width 4))))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.myzshrc\\'" . sh-mode))

(defalias 'yes-or-no-p 'y-or-n-p)		; always (y or n) instead of (yes or no)

;;;; keybinds to functions from myfunctions.el


(global-set-key (kbd "C-c C-s") 'sudo-save)

(global-set-key (kbd "C-c C-e") 'region-execute-python)
(global-set-key (kbd "C-c s") 'region-as-python-string)

(global-set-key (kbd "C-x y") 'xpaste)	; xclipboard paste

(global-set-key (kbd "C-x n") 'switch-to-new-buffer) ; 

(global-set-key (kbd "C-c /") 'temp-buffer) ; switch between current buffer and *scratch* buffer


(global-set-key (kbd "C-c r") 'reload-dotemacs-file) ; reload emacs config

(global-set-key (kbd "C-c g") 'search-google) ; preform a google search of region/input

(global-set-key (kbd "M-Y") 'yank-pop-forward)


;;;; keybinds to emacs functions


(global-unset-key (kbd "C-@"))
(new-onekey
 "fast"
 "C-@"
 '(
	("h"	.	left-char)
	("H"	.	left-word)
	 
	("j"	.	next-line)
	("J"	.	scroll-up)
	 
	("k"	.	previous-line)
	("K"	.	scroll-down)
	 
	("l"	.	right-char)
	("L"	.	right-word)
	 
	("a"	.	move-beginning-of-line)
	("A"	.	backward-sexp)
	 
	("e"	.	move-end-of-line)
	("E"	.	forward-sexp)
	 
	("D"	.	kill-sexp)
	("d"	.	kill-whole-line)
	 
	("y"	.	yank)
	("r"	.	yank-pop)
	("R"	.	yank-pop-forward)
	 
	("u"	.	undo-tree-undo)
	("U"	.	undo-tree-redo)

	("f"	.	ace-jump-word-mode)

	("w"	.	kill-ring-save)
	("W"	.	kill-region)

	(";" . comment-line)

	(":"	.	comment-dwim)

	("n"	.	move-text-down)
	("p"	.	move-text-up)

	("<right>"	.	tabbar-forward-tab)
	("<left>"	.	tabbar-backward-tab)
	("<up>"		.	tabbar-forward-group)
	("<down>"	.	tabbar-backward-group)

	("g"	.	goto-line)
	
	("x"	.	kill-this-buffer)
	
	("t"	.	transpose-chars)
	("T"	.	transpose-words)

	("c"	.	select-line)
	
	("SPC"	.	set-mark-command)

	("<"	.	beginning-of-buffer)
	(">"	.	end-of-buffer)

	("`"	.	next-error)

	("o"	.	other-window)
	("O"	.	previous-multiframe-window)

	("s"	.	save-buffer)
	("S"	.	save-some-buffers)

	("%"	.	split-window-below)
	("|"	.	split-window-right)

	("!"	.	shell-command)

	("."	.	recenter)

	("/"	.	helm-find-files)
	("m"	.	helm-semantic)

	("'"	.	delete-window)
	("\""	.	delete-other-windows)
	)
 "common editor functions bound to fast and comfortable keys"
 )

(new-onekey
 "toggle"
 "C-x t"
 '(
   ("l" . linum-mode)
   ("t" . toggle-truncate-lines)
   ("i" . c-toggle-electric-state)
   ("n" . c-toggle-auto-newline)
   ("r" . read-only-mode)
   ("w" . whitespace-mode)
   ("x" . xterm-mouse-mode)
   ("f" . flycheck-mode)
   )
 "toggle stuff"
 )


(global-set-key (kbd "C-c C-g") 'keyboard-quit) ; quit when misstyped beggining of command
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "C-l") 'recenter)	; less messy than recenter-top-bottom

(global-set-key (kbd "C-o") 'other-window) ; faster windows switching
(global-set-key (kbd "C-x o") (rep 'other-window)) ; activate rep on default window switching

(global-set-key (kbd "C-x C-d") (lambda() "open dired ." (interactive) (dired "."))) ; emacs . for file navigation

(global-set-key (kbd "C-x 5 t") 'term) ; start terminal
(global-set-key (kbd "C-x 5 s") 'eshell) ; start elisp shell

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "M-i") (lambda () "insert tab" (interactive) (insert-tab)))
(global-set-key (kbd "<backtab>") (lambda () "insert tab" (interactive) (insert-tab)))

(global-set-key (kbd "C-x g") 'goto-line) ; goto specified line
(global-set-key (kbd "M-,") 'beginning-of-buffer) ; faster than "M-<"
(global-set-key (kbd "M-.") 'end-of-buffer)		  ; and "M->"

(global-set-key (kbd "C-c m") 'compile)

(global-set-key (kbd "C-c w") 'whitespace-cleanup-region) ; remove trailing whitespaces in region

(global-set-key (kbd "M-m") 'mark-sexp)	; mark balanced expression
(global-set-key (kbd "M-k") 'kill-sexp)	; kill balanced expression

(global-set-key (kbd "M-r") 'isearch-forward-regexp)

(global-set-key (kbd "<f12>") (lambda() (interactive) (switch-to-buffer (get-buffer-create "*scratch*")))) ; go to scratch buffer

(global-set-key (kbd "C-M-j") (lambda() (interactive) (set-mark-command 1))) ; jump to the last marked point
(global-set-key (kbd "M-RET") (lambda() (interactive) (set-mark-command 1))) ; jump to the last marked point

(global-set-key (kbd "M-'") 'repeat)	; repeat command faster

(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-c C-r") 'string-insert-rectangle)

(global-set-key (kbd "M-p") 'scroll-down-command) ; scroll-up Oo
(global-set-key (kbd "M-n") 'scroll-up-command)	  ; scroll down oO

(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)

(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-<left>") 'left-word)

(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "<ESC> <right>") 'forward-sexp)
(global-set-key (kbd "<ESC> <left>") 'backward-sexp)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "<ESC> <up>") 'move-text-up)
(global-set-key (kbd "<ESC> <down>") 'move-text-down)

(global-set-key (kbd "C-v") 'quoted-insert) ; like in the shell


;;;; resize hotkeys

(global-set-key (kbd "C-c <right>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'enlarge-window-horizontally))))
(global-set-key (kbd "C-c <left>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'shrink-window-horizontally))))
(global-set-key (kbd "C-c <down>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'enlarge-window))))
(global-set-key (kbd "C-c <up>") '(lambda () "" (interactive) (progn (setq move-n 1) (setq old-resize-call 1) (dynamic-resize-mode 'shrink-window))))


;; configs and keybinds from modes
(use-package semantic
  :defer 3
  :bind (
		 ("C-x j" . semantic-complete-jump) ; jump to local symbol
		 ("C-c j" . senator-go-to-up-reference) ; jump to definition
		 ("C-c f" . semantic-symref)
		 )
  :config
  (use-package cc-mode)
  (use-package semantic/ia)
  (global-ede-mode 1)                      ; Enable the Project management system
  (semantic-mode 1)						 ;
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)	; update DB wen idle
  (global-semantic-show-parser-state-mode)
  (global-semantic-mru-bookmark-mode)
  (global-semantic-highlight-func-mode)
  (semantic-add-system-include "/data/include" 'c++-mode)
  (semantic-add-system-include "/data/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/" 'c++-mode)
  (semantic-add-system-include "/usr/local/include/" 'c++-mode)
  (semantic-add-system-include "/usr/include/c++/4.9/" 'c++-mode)
  )

(use-package linum						; get line number
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq linum-format "%4d \u2502 ")
  )

(use-package hideshow					; factorize functions {...}
  :bind (
		 ([f5] . hs-hide-all)
		 ([f6] . hs-show-all)
		 ("C-x x" . hs-toggle-hiding)
		 ("M-v" . hs-toggle-hiding)
		 )
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
)

(use-package paren						; show matching parenthese
  :defer 2
  :config
  (show-paren-mode 1)			; ON
  (setq show-paren-delay 0)		; delay
  )

(use-package mouse
  :config
  (xterm-mouse-mode t)					; mouse on mofo
  (global-set-key (kbd "<mouse-2>") 'nil)
  (global-set-key (kbd "<mouse-3>") 'xpaste) ; right clic to paste from xclipboard
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(use-package org-mode
  :defer t
  )

;; (require 'zone)							; kind of screen saver
;; (zone-when-idle 60)						; after 60s

;; this is not vi(m)
(defconst wq "You mean C-x C-c ?")
(defconst qq "You mean C-x C-c ?")
(defconst w "You mean C-x C-s ?")
(defconst q! "You mean C-x C-c ?")
(defconst wq! "You mean C-x C-c ?")
(defconst qw! "You mean C-x C-c ?")

(provide 'myconfig)
;;; myconfig.el ends here
